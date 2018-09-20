
"""*******************************************************

             EUREKA SOURCE CODE FUNCTIONS
                    JUNE 2nd 2018
           Author: Sifael Sebastian Ndandala

Desc: Eureka Source Code
1. Sample Size Estimation
2. Test/Market Segmentation
3. Causal Impact Analysis Generated in Python Port
*******************************************************"""


import math
import numpy as np
import pandas as pd
from scipy.stats import norm
import matplotlib.pyplot as plt


class PropSample:

    def __init__(self, power=.8, significance=.05):
        self.power = power
        self.significance = significance

    def estimateSample(self, ratio, baseline, lift, power=None, significance=None):
        """
        Method Computes Proportion Sample
        Params
            * Ratio: Ratio of treatment groups in prortion
            * Baseline: Observed probability of success i.e. CTR
            * Lift: expected incremental lift
            * Power: Power Level for the test
            * Significance: Significance Level for the test
        """
        self.ratio = ratio
        self.baseline = baseline
        self.lift = lift

        # Check if power and significance level are specified
        if power is None:
            power = self.power
        else:
            pass


        if significance is None:
            significance = self.significance
        else:
            pass

        # New Ratio
        new_ratio = round(self.ratio*10/(1 -self.ratio)/10, 2)

        # Critical Values for Power and Significance
        z_power = norm.ppf(power)
        z_siglevel = abs(norm.ppf(significance/2))
        p_bar = self.baseline + self.lift

        # Calculate the Sample
        sample_ratio = (new_ratio + 1)/new_ratio
        sample_calc = ( (p_bar)*(1-p_bar)*(z_power+ z_siglevel)**2)/(self.lift)**2
        sample = sample_ratio*sample_calc

        return np.round(sample, 0)

    def dailySampleEstimates(self, daily_obs=100):
        """ Returning Daily Sample """
        self.daily_obs = daily_obs

        new_ratio = round(self.ratio*10/(1 -self.ratio)/10, 2)
        sample = self.estimateSample(self.ratio, self.baseline, self.lift, self.power, self.significance)
        data = {'Sample Size': [  int(math.ceil(new_ratio*sample)), int(sample)], 'Days': [ math.ceil(new_ratio*sample/self.daily_obs), math.ceil(sample/self.daily_obs)]}

        return pd.DataFrame(data, index=['Test', 'Control'])


    def samplePowerPlot(self):
        """ Return Sample Plots """

        power_levels = np.arange(.4, 1, .05)
        samples = self.estimateSample(self.ratio, self.baseline, self.lift, power_levels, self.significance)

        fig = plt.figure(figsize=(8,5))
        plt.scatter(samples, power_levels)
        plt.xlabel('Sample Sizes')
        plt.ylabel('Power Level')
        plt.title('Sample vs Power Level, Significance Level: {}'.format(self.significance))
        plt.show()


    def sampleSigPlot(self):
        """ Return Significant Plots """

        sig_levels = np.arange(.01, .3, .01)
        samples = self.estimateSample(self.ratio, self.baseline, self.lift, self.power, sig_levels)

        fig = plt.figure(figsize=(8,5))
        plt.scatter(samples, sig_levels)
        plt.xlabel('Sample Sizes')
        plt.ylabel('Significance Level')
        plt.title('Sample vs Significance Level, Power Level: {}'.format(self.power))
        plt.show()



class MultiPropSample(PropSample):
    """ Multiple Proportion Sample Estimation """

    def __init__(self, input_file):
        self.input_file = input_file

    def estimateMultiSamples(self):
        """ Computing Samples by """
        groups = self.input_file.iloc[:, 0].unique().tolist()

        for group in groups:
            print(group)
