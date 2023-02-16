#!/usr/bin/python

import ctypes
from ctypes import *
import os
import sys
from struct import pack
from io import StringIO
import numpy as np
import time

# Set the directory for your HantekScope DLL here.
marchdll_file = os.path.join("Dll_x64", "HTHardDll.dll")
marchdll_file_graph = os.path.join("Dll_x64", "HTDisplayDll.dll")

class Oscilloscope(object):
    def __init__(self, scopeid=0):  # Set up our scope. The scope id is for each scope attached to the system.
        # No Linux support...yet
        if os.name != 'nt':
            raise StandardError('Hantek SDK Oscilloscope wrapper only supports windows!')

        # self.marchdll = ctypes.CDLL(marchdll_file) #WinDLL(marchdll_file)
        self.marchdll = ctypes.CDLL(marchdll_file) #WinDLL(marchdll_file)
        self.marchdll_graph = ctypes.CDLL(marchdll_file_graph)

        self.scopeid = c_ushort(scopeid)
        self.scop_id = np.array(self.scopeid, ctypes.c_ushort)
        
        self.LeverPos = [128, 128, 128, 128]
        
        self.TimeDIV = 21  #6-2V/div , 21- 10V/div
        self.YTFormat = c_int16(0) #0-normal, 1-scan, 2-roll

        class STRUCT_Control(Structure):
            _fields_ = [("nCHSet", c_int16),
                    ("nTimeDiv", c_int16),
                    ("nTriggerSource", c_int16),
                    ("nHTriggerPos", c_int16),
                    ("nVTriggerPos", c_int16),
                    ("nTriggerSlope", c_int16),
                    ("nBufferLen", c_int32),
                    ("nReadDataLen", c_int32),
                    ("nAlreadyReadLen", c_int32),
                    ("nALT", c_int16)]
        self.stControl = STRUCT_Control()
        self.stControl.nCHSet = 15
        self.stControl.nTimeDiv = self.TimeDIV
        self.stControl.nTriggerSource = 0
        self.stControl.nHTriggerPos = 0 
        self.stControl.nVTriggerPos = self.LeverPos[0]
        self.stControl.nTriggerSlope = 0
        self.stControl.nBufferLen = 4096
        self.stControl.nReadDataLen = 4096
        self.stControl.nAlreadyReadLen = 0
        self.stControl.nALT = 0
        
        self.nTriggerCouple = 1  # 0-DC, 1-AC, 2-LowFreq, 3-HighFreq
        self.DisLen = 4096; 
        self.StartNew = True
      
        class STRUCT_RelayControl(Structure):
            _fields_ = [("bCHEnable", c_int32 * 4),
                        ("nCHVoltDIV", c_int16 * 4),
                        ("nCHCoupling", c_int32 * 4),
                        ("bCHBWLimit", c_int16 * 4),
                        ("nTrigSource", c_int16),
                        ("bTrigFilt", c_int32),
                        ("nALT", c_int16)]
                        
        self.rcRelayControl = STRUCT_RelayControl()
        for i in range(0,4):
            self.rcRelayControl.bCHEnable[i] = 1
            self.rcRelayControl.nCHVoltDIV[i] = 9
            self.rcRelayControl.nCHCoupling[i] = 0 
            self.rcRelayControl.bCHBWLimit[i] = 0
        self.rcRelayControl.nTrigSource = self.stControl.nTriggerSource
        self.rcRelayControl.bTrigFilt = 0
        self.rcRelayControl.nALT = 0
        
        self.TriggerMode = 0 #0-edge, 1-pulse, 2-video
        self.TriggerSlope = self.stControl.nALT #0-increasing slop, 1-decreasing slope

        self.TriggerSweep = 0
        self.ReadOK = 0
        self.StartNew = True
        self.ForceTriggerCnt = 0
        self.Collect = 1
        self.CH1SrcData = (c_short * 4096)()
        self.CH2SrcData = (c_short * 4096)()
        self.CH3SrcData = (c_short * 4096)()
        self.CH4SrcData = (c_short * 4096)()   
        
        self.cal_data = None

    def is_attached(self):
        """
            Takes no arguments.
            Returns true if the scope is attached, false otherwise.
        """
        
        #retval = self.marchdll.dsoOpenDevice(self.scopeid)
        retval = self.marchdll.dsoHTSearchDevice(self.scop_id.ctypes)
        if not retval:
            return False
        elif retval == 1:
            print("1. dsoHTSearchDevice")
            return True
        else:
            print("ERROR: Unexpected return value through API. dsoHTSearchDevice")
            return False


    def initHard(self):
        retval = self.marchdll.dsoInitHard(0)
        if not retval:
            return False
        elif retval == 1:
            print("2. InitHard")
            return True
        else:
            print("ERROR: Unexpected return value through API. InitHard")
            return False

    def HTADCCHModGain(self):
        retval = self.marchdll.dsoHTADCCHModGain(0, c_int16(1))
        if not retval:
            return False
        elif retval == 1:
            print("3. ModGain")
            return True
        else:
            print("ERROR: Unexpected return value through API. ModGain")
            return False

    def HTSetSampleRate(self):
        retval = self.marchdll.dsoHTSetSampleRate(0, byref(self.YTFormat), byref(self.rcRelayControl), byref(self.stControl))
        if not retval:
            return False
        elif retval == 1:
            print("4. HTSetSampleRate")
            return True
        else:
            print("ERROR: Unexpected return value through API. HTSetSampleRate")
            return False

    def HTSetCHAndTrigger(self):
        retval = self.marchdll.dsoHTSetCHAndTrigger(0, byref(self.rcRelayControl), byref(c_int16(self.stControl.nTimeDiv)))
        if not retval:
            return False
        elif retval == 1:
            print("5. HTSetCHAndTrigger")
            return True
        else:
            print("ERROR: Unexpected return value through API. HTSetCHAndTrigger")
            return False

    def HTSetRamAndTrigerControl(self):
        retval = self.marchdll.dsoHTSetRamAndTrigerControl(0, byref(c_int16(self.stControl.nTimeDiv)), byref(c_int16(self.stControl.nCHSet)), byref(c_int16(self.stControl.nTriggerSource)), byref(c_int16(0)))
        if not retval:
            return False
        elif retval == 1:
            print("6. HTSetRamAndTrigerControl")
            return True
        else:
            print("ERROR: Unexpected return value through API. HTSetRamAndTrigerControl")
            return False
    
    def HTSetCHPos(self):
        for i in range(0,4):
            retval = self.marchdll.dsoHTSetCHPos(0, c_int16(self.rcRelayControl.nCHVoltDIV[i]), c_int32(self.LeverPos[i]), c_int16(i), c_int16(4))
        
        if not retval:
            return False
        elif    retval == 1:
            print("7. HTSetCHPos")
            return True
        else:
            print("ERROR: Unexpected return value through API. HTSetCHPos")
            return False
        
    def HTSetVTriggerLevel(self):
        retval = self.marchdll.dsoHTSetVTriggerLevel(0, byref(c_int16(self.LeverPos[0])), byref(c_int16(4)))
        if not retval:
            return False
        elif retval == 1:
            print("8. HTSetVTriggerLevel")
            return True
        else:
            print("ERROR: Unexpected return value through API. HTSetVTriggerLevel")
            return False
            
    def HTSetTrigerMode(self):
        if (self.TriggerMode == 0):
            retval = self.marchdll.dsoHTSetTrigerMode(0, byref(c_int16(self.TriggerMode)), byref(c_int16(self.stControl.nTriggerSlope)), byref(c_int16(self.nTriggerCouple)))
            if not retval:
                return False
            elif retval == 1:
                print("9. HTSetTrigerMode")
                return True
            else:
                print("ERROR: Unexpected return value through API. HTSetTrigerMode")
                return False

    

    #@staticmethod
    def CollectData(self):
        if (self.StartNew): 
            retval = self.marchdll.dsoHTStartCollectData(0, byref(c_int16(1)))
            self.StartNew = False
            if not retval:
                print("FALSE. HTSetTrigerMode")
            #elif retval == 1:
                #print("10. HTSetTrigerMode OK")
                
  
        nState = self.marchdll.dsoHTGetState(0)
        if (nState and 2 == 2 ):
            self.read_data_from_scope(data_points=4096)
            self.StartNew = True
            #print("nState NEW")
        else:
            self.StartNew = False
            print("nState OLD")


    def read_data_from_scope(self, data_points=4096, display_points=4096, raw_data=False):
        """
            Takes two optional arguments, number of data points and number of display point
            to grab. Returns a tuple with channel 1 data, channel 2 data, time since capture init, and a trigger
            index on success, and None on failure.
        """
        #if self.cal_data == None:
         #   return None
        #else:
        '''
        data_ch1 = c_double(0)
        data_ch2 = c_double(0)
        data_ch3 = c_double(0)
        data_ch4 = c_double(0)
        '''
        data_ch1 = (c_short * data_points)()
        data_ch2 = (c_short * data_points)()
        data_ch3 = (c_short * data_points)()
        data_ch4 = (c_short * data_points)()
        
        t_index = c_ulong(0)

        
        #stControl = {'nCHSet': 7, 'nTimeDiv': 21, 'nTriggerSource': 0, 'nHTriggerPos': 0, 'nVTriggerPos': 0, 'nTriggerSlope': 0, 'nBufferLen': 4096,
        #'nReadDataLen': 4096, 'nAlreadyReadLen': 0, 'nALT': 0}
        
    
        retval = self.marchdll.dsoHTGetData(self.scopeid, byref(data_ch1), byref(data_ch2), byref(data_ch3), byref(data_ch4), byref(self.stControl))
        
        #retval = self.marchdll.dsoHTGetData(0, byref(data_ch1), byref(data_ch2), byref(data_ch3), byref(data_ch4), self.stControl.ctypes)
        
        #!!!retval = self.marchdll.dsoHTGetData(0, data_ch1_d.ctypes, data_ch2_d.ctypes, data_ch3_d.ctypes, data_ch4_d.ctypes, self.stControl.ctypes)
        
        if retval == -1:
            return None
        elif raw_data:
            return data_ch1, data_ch2, data_ch3, data_ch4,[j / 1e6 for j in range(0, data_points+1)], t_index
        else:
            for i in range (0, 4096):
                self.CH1SrcData[i] = data_ch1[i] - (255 - self.LeverPos[0])
                self.CH2SrcData[i] = data_ch2[i] - (255 - self.LeverPos[1])
                self.CH3SrcData[i] = data_ch3[i] - (255 - self.LeverPos[2])
                self.CH4SrcData[i] = data_ch4[i] - (255 - self.LeverPos[3])

    def GetDataFromDSO(self):
        t_index = c_ulong(0)
        return self.CH1SrcData, self.CH2SrcData, self.CH3SrcData, self.CH4SrcData, [j / 1e6 for j in range(0, 4097)], t_index

  
