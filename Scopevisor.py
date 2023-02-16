import time
import matplotlib.backends
import matplotlib
matplotlib.use('Qt5Agg') 
import numpy as np
import matplotlib.pylab as plt
import matplotlib.rcsetup as rcsetup
#print(rcsetup.all_backends)
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

    timebase = conversions.TIMEBASE[21]
    timebase_str = utils.format_number(timebase, "s")
    seconds_per_sample = 1 / 10
    time = np.arange(0, float(10))
    time *= seconds_per_sample

    # Darkmode to simulate oscilloscope
    plt.style.use('dark_background')
    fig, ax = plt.subplots()

    # Setup the graph like an oscilloscope graticule
    ax.set_ylim(-250, 250)
    ax.xaxis.set_major_locator(ticker.MultipleLocator(timebase))
    ax.xaxis.set_major_formatter(ticker.NullFormatter())
    ax.xaxis.set_minor_locator(ticker.MultipleLocator(timebase / 5))
    
    ax.yaxis.set_major_formatter(ticker.NullFormatter())
    ax.grid(which="major", axis="both", color="0.5")
    ax.grid(which="minor", axis="both", color="0.2")
    ax.minorticks_on()
    scope0.CollectData()
    
    def animate(i):
        ax.clear()
        length = 300#4096#500 
        #scope0.CollectData()
        scope0.read_data_from_scope(data_points=4096)
        data = scope0.GetDataFromDSO()
        tIndex = data[5].value
        ytrans = transforms.blended_transform_factory(ax.get_yticklabels()[0].get_transform(), ax.transData)
        
        #vpd = conversions.VOLTS_PER_DIV[12]
        #vpd_str = utils.format_number(vpd * (10 ** 1), "V")
        
        #######ax.plot(data[4][tIndex:tIndex + length], data[0][tIndex:tIndex + length], color=conversions.CHANNEL_COLOUR[0]) #, label=f"Ch2: {vpd_str}")
        #ax.axhline(y=-20, color=col, lw=0.8, ls="-")
    
        #vpd = conversions.VOLTS_PER_DIV[12]
        #vpd_str = utils.format_number(vpd * (10 ** 1), "V")
        
        ax.plot(data[4][tIndex:tIndex + length], data[1][tIndex:tIndex + length], color=conversions.CHANNEL_COLOUR[1]) #, label=f"Ch3: {vpd_str}")
        #ax.axhline(y=20, color=col, lw=0.8, ls="-")

        #vpd = conversions.VOLTS_PER_DIV[12]
        #vpd_str = utils.format_number(vpd * (10 ** 1), "V")
        
        ax.plot(data[4][tIndex:tIndex + length], data[2][tIndex:tIndex + length], color=conversions.CHANNEL_COLOUR[2]) #, label=f"Ch4: {vpd_str}")
        #ax.axhline(y=40, color=col, lw=0.8, ls="-")
        
        ax.plot(data[4][tIndex:tIndex + length], data[3][tIndex:tIndex + length], color=conversions.CHANNEL_COLOUR[3]) #, label=f"Ch4: {vpd_str}")
        # Trigger line
        #trigger_level = 0
        #trigger_channel = 1
        #col = conversions.CHANNEL_COLOUR[trigger_channel]
        #ax.axhline(y=trigger_level, color=col, lw=0.8, ls="-")
        #ax.text(0, trigger_level, "T", color=col, transform=ytrans, ha="right", va="center")

        # Timebase Text
        samples_per_second = utils.format_number(1)
        sampling_depth = utils.format_number(1)
        plt.title(f"Timebase: {timebase_str}, {samples_per_second}Sa/s, {sampling_depth}Pt")
        #ax.legend()
        

    ani = animation.FuncAnimation(fig, animate, interval=10)
    plt.show()
    