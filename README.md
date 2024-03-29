# MechaCar_Statistical_Analysis
## Linear Regression to Predict MPG
The analysis of factors that affect the fuel economy of a vehicle included the vehicle’s weight, length, ground clearance, spoiler angle, and drivetrain (2wd or 4wd). The variable that most affected the fuel economy, that is, the variable that provided a non-random amount of variance in the testing was vehicle weight. The coefficient for the vehicle weight variable was 1.245e-3, meaning it was below the assumed p-value, and thus contributed most to the fuel economy of the vehicle. The other coefficients were above the assumed p-value of .05, meaning they were insufficient to reject the null hypothesis, and thus we have to conclude they did not contribute to the fuel economy of the vehicle. 

![this is an image](https://github.com/BartBachrach/MechaCar_Statistical_Analysis/blob/main/Screen%20Shot%202022-06-21%20at%202.30.05%20PM.png)

The slope of the line is largely considered to be zero, although one factor did affect the fuel economy of the vehicle, the other factors did not affect the mpg, and thus the line is mostly zero, with a slight negative slope, as the weight of the vehicle was negatively correlated to the fuel economy of the vehicle.

With the variables tested in this analysis, the model does not effectively predict the fuel economy of the MechaCar prototype, with the exception of vehicle weight. The other variables did not correlate to mpg in any meaningful way, and thus cannot be considered to predict the fuel efficiency of the prototype vehicle. 

## Summary Statistics on Suspension Coils
The design specifications for the MechaCar prototype require that the suspension coils PSI rating remain within 100 PSI of the prescribed 1500 pounds per square inch. Overall, the variance among all three manufacturing lots is 62.29 PSI, which does of course meet the design specifications. 
However, digging deeper into the data on the three manufacturing lots, we find that variance is as high as it is because of manufacturing lot 3. Lot 1 had a tight variance at .97 PSI, and Lot 2 had a higher but still relatively tight variance at 7.47 PSI. Lot 3, however, went off the page at over 170 PSI. This analyst recommends halting further production and investigating how that variance occurred. 

![this is an image](https://github.com/BartBachrach/MechaCar_Statistical_Analysis/blob/main/Screen%20Shot%202022-06-26%20at%206.28.48%20PM.png)

## T-Tests on Suspension Coils
The t-tests for lots 1 and 2 revealed that the mean PSI for suspension coils was not statistically different from the population mean, with the p-value for each being well above the assumed .05. In fact, the p-value for lot 1 was 1, and lot 2 was .6, meaning we have insufficient evidence to reject the null hypothesis that there is no statistical difference between those lots’ mean suspension coil PSI and the population mean of 1500. 
The p-value for lot 3 sits at .04, just under the assumed p-value of .05, meaning there is sufficient evidence to reject the hypothesis that there is no statistical difference between the mean PSI of that group and the population. This is supported by our earlier summaries between the manufacturing lots and the mean PSI of those suspension coils. 

![this is an image](https://github.com/BartBachrach/MechaCar_Statistical_Analysis/blob/main/Screen%20Shot%202022-06-26%20at%206.24.43%20PM.png)

## Study Design: MechaCar vs Competition
The MechaCar prototype will need to be evaluated against the competition, not just it’s own production lots. While many considerations come into play when considering purchasing a new vehicle, such as maintenance cost, cost to insure, and safety rating, one of the primary metrics drivers consider when purchasing a new vehicle is fuel economy. With the rising cost of oil and gas at the pump, fewer things are on the minds of consumers these days. 

MechaCar believes that the fuel economy for its new prototype is statistically higher than the competition so as to set it apart, making it the superior option when considering fuel economy. The alternative being that the prototype is significantly worse on fuel economy than the competition, or it is possible that the fuel economy of the prototype vehicle is about the same as all the others. 

The best way to find out would be to gather multiple vehicles of the same make and model, including multiple MechaCar prototypes and run them through every conceivable vehicle test, driving in the city, suburbs, on the highway, in extreme heat and extreme cold, over rough surfaces and perfectly paved roads, to put the prototype and its competitors through their paces. Once the fuel efficiency data is collected, analysts (perhaps this one) would run t-tests on the data to determine if there is enough evidence to first reject the idea that the prototype is on par with its competitors. We would then determine if there is enough evidence to reject the idea that the fuel economy is statistically worse than the competition on fuel economy. The findings of this study would then inform either the marketing of the vehicle or the engineering team to redesign the vehicle to perform better.
