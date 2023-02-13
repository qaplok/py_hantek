#!/usr/bin/python

import ctypes
from ctypes import *
import os
import sys
from struct import pack
from io import StringIO
import numpy as np


# Set the directory for your HantekScope DLL here.
marchdll_file = os.path.join("Dll_x64", "HTHardDll.dll")
#marchdll_file = os.path.join("Dll_x32", "HTHardDll.dll")

class Oscilloscope(object):
    def __init__(self, scopeid=0):  # Set up our scope. The scope id is for each scope attached to the system.
        # No Linux support...yet
        if os.name != 'nt':
            raise StandardError('Hantek SDK Oscilloscope wrapper only supports windows!')

        # self.marchdll = ctypes.CDLL(marchdll_file) #WinDLL(marchdll_file)
        self.marchdll = ctypes.CDLL(marchdll_file) #WinDLL(marchdll_file)
        
        self.scopeid = c_ushort(scopeid)
        self.scop_id = np.array(self.scopeid, ctypes.c_ushort)
        
        self.LeverPos = [0, 0, 0, 0]
        
        self.TimeDIV = 21; 
        self.YTFormat = 0; 
        #stControl_q = {'nCHSet': 7, 'nTimeDiv': self.TimeDIV, 'nTriggerSource': 0, 'nHTriggerPos': 0, 'nVTriggerPos': self.LeverPos[0], 'nTriggerSlope': 0, 'nBufferLen': 4096,
        #'nReadDataLen': 4096, 'nAlreadyReadLen': 0, 'nALT': 0}
        stControl_q = [7,21,0,0,0,0,4096,4096,0,0]
        self.stControl = np.array(stControl_q, ctypes.c_int16)
        self.DisLen = 2500; 
        self.StartNew = True
            #stControl.nALT = 0
        '''
        self.rcRelayControl = {'bCHEnable': [],  'nCHVoltDIV': [], 'nCHCoupling': [], 'bCHBWLimit': [], 'nTrigSource': self.stControl['nTriggerSource'], 'bTrigFilt': 0, 'nALT': self.stControl["nALT"]}
        for i in range(0,3):
            self.rcRelayControl['bCHEnable'[i]] = 1
            self.rcRelayControl['nCHVoltDIV'[i]] = 8
            self.rcRelayControl['nCHCoupling'[i]] = 0 
            self.rcRelayControl['bCHBWLimit'[i]] = 0
        '''
        rcRelayControl_q = [1,1,1,1,8,8,8,8,0,0,0,0,0,0,0,0,0,0,0]
        self.rcRelayControl = np.array(rcRelayControl_q, ctypes.c_double)
        self.TriggerMode = 0
        self.TriggerSlope = 0
        self.TriggerSweep = 0
        self.ReadOK = 0
        self.StartNew = True
        self.ForceTriggerCnt = 0
        self.Collect = 1
        self.CH1SrcData = (c_short * 4096)()
        self.CH2SrcData = (c_short * 4096)()
        self.CH3SrcData = (c_short * 4096)()
        self.CH4SrcData = (c_short * 4096)()   
        self.pAmpLevel = np.zeros(579)
        for i in range(0, 578):
            self.pAmpLevel[i] = 1024

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
        mod = 4
        mod_d = np.array(mod, ctypes.c_double)
        retval = self.marchdll.dsoHTADCCHModGain(0, mod_d.ctypes)
        if not retval:
            return False
        elif retval == 1:
            print("3. ModGain")
            return True
        else:
            print("ERROR: Unexpected return value through API. ModGain")
            return False
    def HTSetSampleRate(self):
        SR = 0
        SR_d = np.array(SR, ctypes.c_double)
        retval = self.marchdll.dsoHTSetSampleRate(0, SR_d.ctypes, self.rcRelayControl.ctypes, self.stControl.ctypes)
        if not retval:
            return False
        elif retval == 1:
            print("4. HTSetSampleRate")
            return True
        else:
            print("ERROR: Unexpected return value through API. HTSetSampleRate")
            return False

    def HTSetCHAndTrigger(self):
        CH = 21
        CH_d = np.array(CH, ctypes.c_double)
        retval = self.marchdll.dsoHTSetCHAndTrigger(0, self.rcRelayControl.ctypes, CH_d.ctypes)
        if not retval:
            return False
        elif retval == 1:
            print("5. HTSetCHAndTrigger")
            return True
        else:
            print("ERROR: Unexpected return value through API. HTSetCHAndTrigger")
            return False

    def HTSetRamAndTrigerControl(self):
        Ram = [21, 7, 0, 0]
        Ram_d = np.array(Ram, ctypes.c_double)
        retval = self.marchdll.dsoHTSetRamAndTrigerControl(0, Ram_d.ctypes)
        if not retval:
            return False
        elif retval == 1:
            print("6. HTSetRamAndTrigerControl")
            return True
        else:
            print("ERROR: Unexpected return value through API. HTSetRamAndTrigerControl")
            return False
    
    def HTSetCHPos(self):
        CHPos1 = [8, 0, 0, 4]
        CHPos2 = [8, 0, 1, 4]
        CHPos3 = [8, 0, 2, 4]
        CHPos4 = [8, 0, 3, 4]
        CHPos1_d = np.array(CHPos1, ctypes.c_double)
        CHPos2_d = np.array(CHPos2, ctypes.c_double)
        CHPos3_d = np.array(CHPos3, ctypes.c_double)
        CHPos4_d = np.array(CHPos4, ctypes.c_double)

        retval = self.marchdll.dsoHTSetCHPos(0, 8, 0, 0, 4)
        retval = self.marchdll.dsoHTSetCHPos(0, 8, 0, 1, 4)
        retval = self.marchdll.dsoHTSetCHPos(0, 8, 0, 2, 4)
        retval = self.marchdll.dsoHTSetCHPos(0, 8, 0, 3, 4)
        if not retval:
            return False
        elif    retval == 1:
            print("7. HTSetCHPos")
            return True
        else:
            print("ERROR: Unexpected return value through API. HTSetCHPos")
            return False
        
    def HTSetVTriggerLevel(self):
        VT = [0, 4]
        VT_d = np.array(VT, ctypes.c_double)
        retval = self.marchdll.dsoHTSetVTriggerLevel(0, VT_d.ctypes)
        if not retval:
            return False
        elif retval == 1:
            print("8. HTSetVTriggerLevel")
            return True
        else:
            print("ERROR: Unexpected return value through API. HTSetVTriggerLevel")
            return False
            
    def HTSetTrigerMode(self):
        zer = [0, 0, 0]
        zer_d = np.array(zer, ctypes.c_double)
        if (self.TriggerMode == 0):
            retval = self.marchdll.dsoHTSetTrigerMode(0, zer_d.ctypes)
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
        col = [0, 0, 1]
        col_d = np.array(col, ctypes.c_double)
        if (self.StartNew): 
            retval = self.marchdll.dsoHTStartCollectData(0, col_d.ctypes)
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
        
    
        retval = self.marchdll.dsoHTGetData(self.scopeid, byref(data_ch1), byref(data_ch2), byref(data_ch3), byref(data_ch4), self.stControl.ctypes)
        data_ch1_d = np.array(data_ch1, ctypes.c_short)
        data_ch2_d = np.array(data_ch2, ctypes.c_short)
        data_ch3_d = np.array(data_ch3, ctypes.c_short)
        data_ch4_d = np.array(data_ch4, ctypes.c_short)
                
        #retval = self.marchdll.dsoHTGetData(0, byref(data_ch1), byref(data_ch2), byref(data_ch3), byref(data_ch4), self.stControl.ctypes)
        
        #!!!retval = self.marchdll.dsoHTGetData(0, data_ch1_d.ctypes, data_ch2_d.ctypes, data_ch3_d.ctypes, data_ch4_d.ctypes, self.stControl.ctypes)
        
        if retval == -1:
            return None
        elif raw_data:
            return data_ch1, data_ch2, data_ch3, data_ch4,[j / 1e6 for j in range(0, data_points)], t_index
        else:
            for i in range (0, 4096):
                self.CH1SrcData[i] = data_ch1[i] - (255 - self.LeverPos[0])
                self.CH2SrcData[i] = data_ch2[i] - (255 - self.LeverPos[1])
                self.CH3SrcData[i] = data_ch3[i] - (255 - self.LeverPos[2])
                self.CH4SrcData[i] = data_ch4[i] - (255 - self.LeverPos[3])

    def GetDataFromDSO(self):
        t_index = c_ulong(0)
        return self.CH1SrcData, self.CH2SrcData, self.CH3SrcData, self.CH4SrcData, [j / 1e6 for j in range(0, 4096)], t_index
