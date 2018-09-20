
from eureka_functions import *
from eureka_functions import PropSample, MultiPropSample


if __name__ == "__main__":
    print("Main function")

    # Instance of the Sample Function
    sample_function = PropSample(power=.8, significance=.01)

    sample = sample_function.estimateSample(ratio=.5, baseline=.2, lift=.01)
    #print(sample)

    sample = sample_function.estimateSample(ratio=.7,baseline=.2, lift=.01, power=.7, significance=.05)
    #print(sample)

    # Sample By Day
    sample_by_day = sample_function.dailySampleEstimates()
    #print(sample_by_day)

    sample_by_day = sample_function.dailySampleEstimates(200)
    #print(sample_by_day)

    # Multiple Proportion Sample
    #sample_function.samplePowerPlot()
    #sample_function.sampleSigPlot()


    # Multiple Estimation
    data = pd.read_csv('SampleProTest_demo.csv')

    multisample_function = MultiPropSample(data)
    multisample_function.estimateMultiSamples()
