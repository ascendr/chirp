# Copyright 2025 Mike Iacovacci <ascendr@linuxmail.org>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import struct
import logging
import time
from chirp import chirp_common, directory, memmap
from chirp import bitwise, errors
from chirp.settings import RadioSetting, RadioSettingGroup, \
    RadioSettingValueInteger, RadioSettingValueList, \
    RadioSettingValueBoolean, RadioSettingValueString, \
    RadioSettings, RadioSettingValueMap

LOG = logging.getLogger(__name__)

MEM_FORMAT = """
# seekto 0x0080;
struct{
    lbcd        rx_freq[4];
    lbcd        tx_freq[4];
    bbcd        rx_tone[2];
    bbcd       tx_tone[2];
    u8          unk_0x0d[4];
    u8          power:1,
                unk_0x10:1,
                bandwidth:1,
                unk1_0x10:3,
                invert:1,
                talkaround:1;
    u8          pttid_mode:4,
                sqlch_mode:4;
    u8          signaling:4,
                jump_mode:4;
    u8          unk_0x13:2,
                launch_banned:1,
                unk1_0x13:5;
    u8          unk_0x14_18[5];
    u8          dtmf;
    u8          twotone;
    u8          unk_0x1b[1];
    u8          mdc;
    u8          unk_0x1d[1];
    u8          emerg_sys;
    u8          unk_0x1f[1];
    char        name[16];
} memory[10];

# seekto 0x7980;
struct{
	u8			band_a_wrkmode;
	u8			band_b_wrkmode;
	u8			unk_0x82_0x84[3];
	u8			voice;
	u8			band_a_workzone;
	u8			band_b_workzone;
	u8			backlight_tm;
	u8			tone_lvl;
	u8			band_a_chandisp:4,
				band_b_chandisp:4;
	u8			dbl_waiting;
	u8			main_band;
	u8			squelch;
	u8			vox_lvl;
	u8			vox_dly_detect;
	u8			power_save_off;
	u8			power_save_time;
	u8			lone_wrk_remind;
	u8			lone_wrk;
	u8			apo;
	u8			tot;
	u8			tot_alert;
	u8			unk_0x97;
	u8			gps_zone;
	u8			unk_0x99;
	u8			1750hz;
	u8			unk_0x9b_9d[3];
	u8			noaa_chan;
	u8			unk_0x_9f;
	u8			busy_lock:1,
				unk_0xa0:1,
				voice_chinese:1,
				voice_english:1,
				falling_alarm:1,
				alone_worker:1,
				aprs:1,
				vox:1;
	u8			auto_keypad;
	u8			tone;
	u8			unk_0xa3;
	u8			gps;
	u8			power_on_char:1,
				power_on_volt:1,
				lang_cn:1,
				wx:1,
				eng_mode:1,
				unk_0xa5:3;
	u8			tail_mode:3,
				noaa:1,
				disp_reverse:1,
				unk_0xa6:2,
				enhance_func:1;
} settings;

# seekto 0x79b0;
struct{
	u8		pf1_short; 
	u8		pf2_short;
	u8		pf2_long;
	u8		pf2_long;
}prog_key;

# seekto 0x79c0;
struct{
	u8		power_on_pwd[8];
	u8		program_pwd[8];
	u8		power_on_char[8];
} power_on;
"""

def raw_send(serial, data, exlen):
    serial.write(data)
    return(serial.read(exlen))

def wakeup(serial):
    cmd = struct.pack('>QLL',0,0,0xffffffff)
    raw_send(serial,cmd,0)
    time.sleep(.5)
    resp = raw_send(serial,cmd,1)
    if len(resp) != 1 or resp != b'\x41':
        err = f"Radio Wakeup error expected 0x41 got {resp}"
        LOG.error(err)
        raise errors.RadioError(err)

def exit_prog(serial, xor):
    ba = [ b ^ xor for b in b'END\x00']
    resp = raw_send(serial, bytes(ba), 1)
    if (resp[0] ^ xor) != 0x41:
        err = f"Exit Prog expect 0x41 got {resp}"
        LOG.error(err)

def do_init(serial,xor):
    cmd = b'PROGRAM' + bytes([xor])
    resp = raw_send(serial,cmd,1)
    if (resp[0] ^ xor) != 0x41:
        err = f"Init Error expected 0x41 got {resp}"
        LOG.error(err)
        raise errors.RadioError(err)
    xb = xor ^ 0xff
    cmd = bytes([xb] * 8)
    resp = raw_send(serial,cmd,1)
    if (resp[0] ^ xor) != 0x41:
        err = f"Init Error expected 0x41 got {resp}"
        LOG.error(err)
        raise errors.RadioError(err)
    xs = [(xor ^ ord(c)) for c in 'INFORMATION']
    resp = raw_send(serial, bytes(xs),16)
    id = [chr(b ^ xor) for b in resp[:6]]
    if "".join(id) !='BF-5RH':
        err = f"Radio Mismatch got {id}"
        LOG.error(err)
        raise errors.RadioError(err)
    print(f"INFO: {resp.hex()}")
    resp = raw_send(serial,bytes(0x52 ^ xor), 1)
    if (resp[0] ^ xor) != 0x41:
        err = f"Init Error expected 0x41 got 0x{resp.hex()}"
        LOG.error(err)
        raise errors.RadioError(err)

def read_block(serial, xor, addr,blk_len):
    i_cmd = (0x52000000 + addr) ^ (0x01010101 * xor)
    cmd = struct.pack('>L',i_cmd)
    print(f"read: {cmd.hex()}")
    resp = raw_send(serial, cmd, blk_len + 4)
    print(f"recieved {hex(len(resp))} bytes")
    print(f"last 8byte {resp[-8:].hex()}")
    print(resp[:16].hex())
    i_resp = struct.unpack('>L', resp[:4])[0]
    print(hex(i_resp))
    if i_resp != (0x57000000 + addr) ^ (0x1010101 * xor):
        err = "Read block echo failed"
        LOG.error(err)
        raise errors.RadioError(err)
    return resp[4:] 
    

def do_download(radio):
    data = b''
    """
    image_path = "C:\\Users\\user\\Projects\\ChirpDEV\\chirp\\tests\\images\\uv5rhpro_b.img"
    with open(image_path,'rb') as r:
            data = r.read()
    """
    try:
        serial = radio.pipe
        serial.timeout = 5.0
        status = chirp_common.Status()
        status.msg = "Connecting to Radio..."
        radio.status_fn(status)
        wakeup(serial)
        xor = 0x26
        do_init(serial, xor)
        READ_ADDRS = [0x000000, 0x100000, 0x200000, 0x300000,
                    0x400000, 0x500000, 0x600000, 0x700000,
                    0x800000, 0x900000, 0xa00000]
        status.max = len(READ_ADDRS) * 0x1000
        status.msg = "Downloading..."
        for addr in READ_ADDRS:
            resp = read_block(serial, xor, addr, 0x1000)    
            da = [b ^ xor for b in resp]
            data += bytes(da)
            status.cur += len(resp)
            radio.status_fn(status)
    except Exception as e:
        print(f"Error downloading {e}") 
    return memmap.MemoryMapBytes(data)

def do_upload(radio):
    raise errors.RadioError("Upload not implemented")


@directory.register
class BF5RHPro(chirp_common.CloneModeRadio):
    """Baofeng BF-5RH Pro GPS"""
    VENDOR = "Baofeng"
    MODEL = "BF-5RHPro"
    BAUD_RATE = 19200
    POWER_LEVELS = [chirp_common.PowerLevel("Low",  watts=0.50),
                    chirp_common.PowerLevel("High", watts=3.00)]
    VALID_MODES = ["NFM", "FM"]
    VALID_BANDS = [(136000000, 174000000), (400000000, 470000000)]
    VALID_TONES = (63.0, ) + chirp_common.TONES
    VALID_DCS = (17, 50, 645) + chirp_common.DTCS_CODES
    VALID_CHARSET = "".join(chr(i) for i in range(32, 127))
    VALID_DTMF = [str(i) for i in range(0, 10)] + \
        ["A", "B", "C", "D", "*", "#"]
    ASCII_NUM = [str(i) for i in range(10)] + [' ']
    VALID_STEPS = [2.5, 5.0, 6.25, 10, 12.5, 20, 25, 50]

    def get_features(self):
        rf = chirp_common.RadioFeatures()
        rf.has_settings = True
        rf.has_bank = False
        rf.has_tuning_step = True
        rf.valid_tuning_steps = self.VALID_STEPS
        rf.has_name = True
        rf.valid_characters = self.VALID_CHARSET
        rf.valid_name_length = 6
        rf.has_offset = True
        rf.has_mode = True
        rf.has_dtcs = True
        rf.has_rx_dtcs = True
        rf.has_dtcs_polarity = True
        rf.has_ctone = True
        rf.has_cross = True
        rf.valid_tones = self.VALID_TONES
        rf.can_odd_split = False
        rf.can_delete = True
        rf.valid_modes = self.VALID_MODES
        rf.valid_duplexes = ["", "-", "+", "off"]
        rf.valid_tmodes = ["", "Tone", "TSQL", "DTCS", "Cross"]
        rf.valid_cross_modes = [
            "Tone->DTCS",
            "DTCS->Tone",
            "->Tone",
            "Tone->Tone",
            "->DTCS",
            "DTCS->",
            "DTCS->DTCS"]
        rf.valid_power_levels = self.POWER_LEVELS
        rf.valid_skips = ["", "S"]
        rf.valid_bands = self.VALID_BANDS
        rf.memory_bounds = (1, 10)
        return rf    


    def process_mmap(self):
        self._memobj = bitwise.parse(MEM_FORMAT, self._mmap)

    def sync_in(self):
        try:
            data = do_download(self)
        except errors.RadioError:
            raise
        except Exception as e:
            err = f'Error during download {e}'
            LOG.error(err)
            raise errors.RadioError(err)
        self._mmap = data
        self.process_mmap()

    def sync_out(self):
        do_upload(self)
    
    def get_memory(self, number):
        mem = chirp_common.Memory()
        mem.number = number
        _mem = self._memobj.memory[number-1]
        _name = "".join(str(c) if str(c) in self.VALID_CHARSET else ' ' for c in _mem.name )
        mem.name = _name.rstrip()
        mem.freq = int(_mem.rx_freq) * 10
        mem.power = self.POWER_LEVELS[_mem.power]
        mem.mode = self.VALID_MODES[_mem.bandwidth]

        if int(_mem.tx_freq) == '0':
            mem.duplex = ''
            mem.offset = 0
        elif int(_mem.tx_freq) > int(_mem.rx_freq):
            mem.duplex = '+'
            mem.offset = (int(_mem.tx_freq) - int(_mem.rx_freq)) * 10
        elif int(_mem.rx_freq) > int(_mem.tx_freq):
            mem.duplex = '-'
            mem.offset = (int(_mem.tx_freq) - int(_mem.rx_freq)) * 10

        self.get_tones(_mem, mem)


        mem.extra = RadioSettingGroup("Extra", "extra")

        rstype = RadioSettingValueBoolean(_mem.invert)
        rs = RadioSetting("invert", "TX Invert", rstype)
        mem.extra.append(rs)

        rstype = RadioSettingValueBoolean(_mem.talkaround)
        rs = RadioSetting("talkaround", "Talkaround", rstype)
        mem.extra.append(rs)

        _options = [('Off', 0), ('PTT BOT', 1), ('PTT EOT', 2),
                    ('PTT Both', 3), ('5-Tone BOT', 4),
                    ('5-Tone EOT', 8), ('5-Tone Both', 12)]
        rstype = RadioSettingValueMap(_options, _mem.pttid_mode)
        rs = RadioSetting("pttid_mode", "PTT/5-Tone ID", rstype)
        mem.extra.append(rs)

        _options = [('Off', 0), ('CTDCS', 1), ('Optional', 2),
                    ('Both', 3)]
        rstype = RadioSettingValueMap(_options, _mem.sqlch_mode)
        rs = RadioSetting("sqlch_mode", "Squelch Mode", rstype)
        mem.extra.append(rs)

        _options = [('Off', 0), ('DTMF', 2), ('2-Tone', 4),
                    ('5-Tone', 6), ('MDC', 8), ('BIIS', 12)]
        rstype = RadioSettingValueMap(_options, _mem.signaling)
        rs = RadioSetting("signaling", "Signaling", rstype)
        mem.extra.append(rs)

        _options = [('Off', 0), ('Normal', 1), ('Strict', 2)]
        rstype = RadioSettingValueMap(_options, _mem.jump_mode)
        rs = RadioSetting("jump_mode", "Jump Freq.", rstype)
        mem.extra.append(rs)

        rstype = RadioSettingValueBoolean(_mem.launch_banned)
        rs = RadioSetting("launch_banned", "Launch Banned", rstype)
        mem.extra.append(rs)

        _options = [i for i in range(16+1)]
        rstype = RadioSettingValueList(options=_options, current=int(_mem.dtmf))
        rs = RadioSetting("dtmf", "DTMF Index", rstype)
        mem.extra.append(rs)

        rstype = RadioSettingValueList(options=_options, current=int(_mem.twotone))
        rs = RadioSetting("twotone", "2-Tone Index", rstype)
        mem.extra.append(rs)
        
        _options = [i for i in range(5+1)]
        rstype = RadioSettingValueList(options=_options, current=int(_mem.mdc))
        rs = RadioSetting("mdc", "MDC Index", rstype)
        mem.extra.append(rs)

        _options = [i for i in range(10+1)]
        rstype = RadioSettingValueList(options=_options, current=int(_mem.emerg_sys))
        rs = RadioSetting("emerg_sys", "Emergency System", rstype)
        mem.extra.append(rs)


        return mem
    

    
    def set_memory(self, mem):
        _mem = self._memobj.memory[mem.number - 1]
        
        if mem.empty:
            return
        
        _mem.rx_freq = mem.freq // 10
        _name = [c for c in mem.name]
        if len(_name) < 16:
            _name = _name + [' '] * (16 - len(_name))
        _mem.name = _name
        
        self.set_tone(_mem, mem)

        if mem.duplex == '+':
            _mem.tx_freq = (mem.freq + mem.offset) // 10
        elif mem.duplex == '-':
            _mem.tx_freq = (mem.freq - mem.offset) // 10
        elif mem.duplex == '':
            _mem.tx_freq == 0x0000

        for setting in mem.extra:
            setattr(_mem, setting.get_name(), setting.value)
    
    def get_tones(self, _mem, mem):
        # populate ui from _mem
        # parse decode/rtone/rx_dtcs
        _rx_tone = int(_mem.rx_tone)
        if _rx_tone == 0:
            # off
            rxtone = ("", 0, None)
        elif _rx_tone < 8000:
            # ctcss tone
            rxtone = ("Tone", _rx_tone / 10, None)
        elif _rx_tone >= 8000 and _rx_tone < 12000:
            # DCS Normal
            rxtone = ("DTCS", (_rx_tone - 8000), "N")
        elif _rx_tone >= 12000:
            # DCS Inverted
            rxtone = ("DTCS", (_rx_tone - 12000), "R")
        else:
            rxtone = ("", 0, None)
        # parse encode/dtcs/ctone
        _tx_tone = int(_mem.tx_tone)
        if _tx_tone == 0:
            # off
            txtone = ("", 0, None)
        elif _tx_tone < 8000:
            # ctcss tone
            txtone = ("Tone", _tx_tone / 10, None)
        elif _tx_tone >= 8000 and _tx_tone < 12000:
            # DCS Normal
            txtone = ("DTCS", (_tx_tone - 8000), "N")
        elif _tx_tone >= 12000:
            # DCS Inverted
            txtone = ("DTCS", (_tx_tone - 12000), "R")
        else:
            txtone = ("", 0, None)
        chirp_common.split_tone_decode(mem, txtone, rxtone)
    
    def set_tone(self, _mem, mem):
         # sets tones in _mem from ui edit
        ((txmode, txval, txpol),
         (rxmode, rxval, rxpol)) = chirp_common.split_tone_encode(mem)
        if txmode == "":
            _mem.tx_tone = 0x0000
        if rxmode == "":
            _mem.rx_tone = 0x0000
        if txmode == "Tone":
            _mem.tx_tone = txval * 10
        if rxmode == "Tone":
            _mem.rx_tone = rxval * 10
        if txmode == "DTCS" and txpol == "N":
            _mem.tx_tone = txval + 8000
        if rxmode == "DTCS" and rxpol == "N":
            _mem.rx_tone = rxval + 8000
        if txmode == "DTCS" and txpol == "R":
            _mem.tx_tone = txval + 12000
        if rxmode == "DTCS" and rxpol == "R":
            _mem.rx_tone = rxval + 12000


    def get_settings(self):

        _settings = self._memobj.settings

        settings = RadioSettingGroup("settings", "Settings")
        group = RadioSettings(settings)

        rs = RadioSettingValueInteger(minval=0, maxval=9,
                                      current=_settings.squelch, step=1)
        rset = RadioSetting("settings.squelch", "Squelch Level", rs)
        settings.append(rset)

        rs = RadioSettingValueInteger(minval=0, maxval=9,
                                      current=_settings.vox_lvl, step=1)
        rset = RadioSetting("settings.vox_lvl", "Vox Level", rs)
        settings.append(rset)

        rs = RadioSettingValueInteger(minval=0, maxval=5,
                                      current=_settings.tone_lvl, step=1)
        rset = RadioSetting("settings.tone_lvl", "Tone Level", rs)
        settings.append(rset)
        
        rs = RadioSettingValueBoolean(
            current=_settings.vox, mem_vals=(0, 1))
        rset = RadioSetting("settings.vox", "VOX", rs)
        settings.append(rset)

        rs = RadioSettingValueBoolean(
            current=_settings.wx, mem_vals=(0, 1))
        rset = RadioSetting("settings.wx", "WX", rs)
        settings.append(rset)

        rs = RadioSettingValueBoolean(
            current=_settings.noaa, mem_vals=(0, 1))
        rset = RadioSetting("settings.noaa", "NOAA", rs)
        settings.append(rset)

        _options = [str(i) for i in range(1, 10+1)]
        rs = RadioSettingValueList(
            _options, current_index=_settings.noaa_chan)
        rset = RadioSetting("settings.noaa_chan",
                            "NOAA Channel", rs)
        settings.append(rset)

        rs = RadioSettingValueBoolean(
            current=_settings.gps, mem_vals=(0, 1))
        rset = RadioSetting("settings.gps", "GPS", rs)
        settings.append(rset)

        _options = [str(i) for i in range(-12,13,1)]
        rs = RadioSettingValueList(
            _options, current_index=_settings.gps_zone)
        rset = RadioSetting("settings.gps_zone",
                            "GPS Zone", rs)
        settings.append(rset)

        rs = RadioSettingValueBoolean(
            current=_settings.aprs, mem_vals=(0, 1))
        rset = RadioSetting("settings.aprs", "APRS", rs)
        settings.append(rset)

        _options = ['Off', 'Chinese', 'English']
        rs = RadioSettingValueList(
            _options, current_index=_settings.voice)
        rset = RadioSetting("settings.voice",
                            "Voice Announce", rs)
        settings.append(rset)

        return group
    
    def set_settings(self, settings):
        for element in settings:
            if not isinstance(element, RadioSetting):
                self.set_settings(element)
                continue
            else:
                try:
                    if "." in element.get_name():
                        toks = element.get_name().split(".")
                        obj = self._memobj
                        for tok in toks[:-1]:
                            if '[' in tok:
                                t, i = tok.split("[")
                                i = int(i[:-1])
                                obj = getattr(obj, t)[i]
                            else:
                                obj = getattr(obj, tok)
                        setting = toks[-1]
                    else:
                        obj = self._memobj.settings
                        setting = element.get_name()

                    if element.has_apply_callback():
                        LOG.debug("applying callback")
                        element.run_apply_callback()

                    if element.value.get_mutable():
                        if obj._name in ['dtmf_list', 'dtmf_start', 'dtmf_end',
                                         'remote_stun', 'remote_kill']:
                            self.dtmf_set__mem(obj, setting, element)
                        else:
                            setattr(obj, setting, element.value)
                except Exception:
                    LOG.debug(element.get_name())
                    raise