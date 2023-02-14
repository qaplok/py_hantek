import time
import matplotlib.backends
import matplotlib
matplotlib.use('Qt5Agg') 
import numpy as np
import matplotlib.pylab as plt
import matplotlib.rcsetup as rcsetup
print(rcsetup.all_backends)
from matplotlib.figure import Figure
import matplotlib.animation as animation
from HTSDKScope_my import Oscilloscope
from collections import deque        
from PIL import Image
import ctypes
import os
import matplotlib.transforms as transforms
import conversions, utils
import matplotlib.ticker as ticker

if __name__ == "__main__":
    marchdll_file_graph = os.path.join("Dll_x64", "HTDisplayDll.dll")
    marchdll_graph = ctypes.CDLL(marchdll_file_graph)

    #['GTK3Agg', 'GTK3Cairo', 'GTK4Agg', 'GTK4Cairo', 'MacOSX', 'nbAgg', 'QtAgg', 'QtCairo', 'Qt5Agg', 'Qt5Cairo', 'TkAgg', 
    # 'TkCairo', 'WebAgg', 'WX', 'WXAgg', 'WXCairo', 'agg', 'cairo', 'pdf', 'pgf', 'ps', 'svg', 'template']
    scope0 = Oscilloscope(scopeid=0)
    if not scope0.is_attached():
        print("WARNING: Scope not found!")
        exit()
    
    scope0.initHard()
    scope0.HTADCCHModGain()
    scope0.HTSetSampleRate()
    scope0.HTSetCHAndTrigger()
    scope0.HTSetRamAndTrigerControl()
    scope0.HTSetCHPos()
    scope0.HTSetVTriggerLevel()
    scope0.HTSetTrigerMode()
    
    scope0.CollectData()
    data = scope0.GetDataFromDSO()
    tIndex = data[5].value

    length = 4096

    timebase = conversions.TIMEBASE[30]
    timebase_str = utils.format_number(timebase, "s")
    seconds_per_sample = 1 / 23
    time = np.arange(0, float(23))
    time *= seconds_per_sample

    # Darkmode to simulate oscilloscope
    plt.style.use('dark_background')
    fig, ax = plt.subplots()

    # Setup the graph like an oscilloscope graticule
    ax.set_ylim(-100, 100)
    ax.xaxis.set_major_locator(ticker.MultipleLocator(timebase))
    ax.xaxis.set_major_formatter(ticker.NullFormatter())
    ax.xaxis.set_minor_locator(ticker.MultipleLocator(timebase / 5))
    ax.yaxis.set_major_formatter(ticker.NullFormatter())
    ax.grid(which="major", axis="both", color="0.5")
    ax.grid(which="minor", axis="both", color="0.2")
    ax.minorticks_on()

    # Plot the channels
    ytrans = transforms.blended_transform_factory(ax.get_yticklabels()[0].get_transform(), ax.transData)
    if len(data[1]) > 0:
        vpd = conversions.VOLTS_PER_DIV[12]
        vpd_str = utils.format_number(vpd * (10 ** 1), "V")
        col = conversions.CHANNEL_COLOUR[0]

        ax.plot(data[4][tIndex:tIndex + length], data[1], color=col, label=f"Ch1: {vpd_str}")
        ax.axhline(y=-80, color=col, lw=0.8, ls="-")
        
    if len(data[2]) > 0:
        vpd = conversions.VOLTS_PER_DIV[12]
        vpd_str = utils.format_number(vpd * (10 ** 1), "V")
        col = conversions.CHANNEL_COLOUR[1]

        ax.plot(data[4][tIndex:tIndex + length], data[2], color=col, label=f"Ch2: {vpd_str}")
        ax.axhline(y=-20, color=col, lw=0.8, ls="-")
        
    if len(data[3]) > 0:
        vpd = conversions.VOLTS_PER_DIV[12]
        vpd_str = utils.format_number(vpd * (10 ** 1), "V")
        col = conversions.CHANNEL_COLOUR[2]

        ax.plot(data[4][tIndex:tIndex + length], data[3], color=col, label=f"Ch3: {vpd_str}")
        ax.axhline(y=20, color=col, lw=0.8, ls="-")
        
    if len(data[4]) > 0:
        vpd = conversions.VOLTS_PER_DIV[12]
        vpd_str = utils.format_number(vpd * (10 ** 1), "V")
        col = conversions.CHANNEL_COLOUR[3]

        ax.plot(data[4][tIndex:tIndex + length], data[4], color=col, label=f"Ch4: {vpd_str}")
        ax.axhline(y=40, color=col, lw=0.8, ls="-")
        

    # Trigger line
    trigger_level = 0
    trigger_channel = 0
    col = conversions.CHANNEL_COLOUR[trigger_channel]
    ax.axhline(y=trigger_level, color=col, lw=0.8, ls="-")
    ax.text(0, trigger_level, "T", color=col, transform=ytrans, ha="right", va="center")

    # Timebase Text
    samples_per_second = utils.format_number(1)
    sampling_depth = utils.format_number(1)
    plt.title(f"Timebase: {timebase_str}, {samples_per_second}Sa/s, {sampling_depth}Pt")

    # Display it
    ax.legend()
    plt.show() 
    #marchdll_graph.HTDrawGrid(hdc, 15, 15, 285, 185, 10, 8, 200, 1)
    #marchdll_graph.HTDrawWaveInYTVB(hdc, 15, 15, 285, 185, 255, 255, 0, 0, data[1], scope0.stControl.nReadDataLen, scope0.DisLen, round(scope0.stControl.nReadDataLen / 2), scope0.LeverPos[0], 1, 1, 0, scope0.stControl.nAlreadyReadLen)

    '''
    fig, ax = plt.subplots(figsize=(6, 4))
    fig.set_facecolor('gray')
    ax.set_facecolor('white')
    ax.set_xlim([0, 4095])
    ax.set_ylim([-270, 270])
    
    ax.autoscale_view(False, False, False)
    line, = ax.plot([], [], lw=30)

   

    def animate(i):
        length = 4096
        scope0.CollectData()
        data = scope0.GetDataFromDSO()
        tIndex = data[5].value
       
        #x.append(x[-1] + 1)  # update data
        #y.append(y[-1] + dy)
        y = data[1][:length]
        x = data[4][tIndex:tIndex + length]
        #line.set_data(x, y)
        ax.clear()
        ax.plot(x,y)
      
    
    
    


    ani = animation.FuncAnimation(fig, animate, interval=50)
    plt.show()
    '''



#marchdll_graph.HTDrawGrid(hDc, 15, 15, 285, 185, 10, 8, 200, 1))
#marchdll_graph.HTDrawWaveInYTVB(hdc, 15, 15, 285, 185, 255, 255, 0, 0, data[1], scope0.stControl.nReadDataLen, scope0.DisLen, round(scope0.stControl.nReadDataLen / 2), scope0.LeverPos[0], 1, 1, 0, scope0.stControl.nAlreadyReadLen)
