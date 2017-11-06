# Read in the data
bankdata = read.table("http://www.stat.cmu.edu/~cschafer/MSCF/OlmedaData.txt")

# Name the variables
names(bankdata) = c("assets_tot","assetMcash_tot","assets_loans",
                        'reverse_loans','income_assets','income_equity',
                        'income_loans','cost_sales','cf_loans','default')

# Change the default variable into 0/1
bankdata$default = as.numeric(bankdata$default == "Failed")

# This function allows for direct reference to the named columns of
# the data frame bankdata. So, instead of writing bankdata$assets_tot,
# I can just write assets_tot

attach(bankdata)

# We need the package car for the Yeo-Johnson Transformation
install.packages("car")
library(car)
# Apply all of the transformations

bankdata$reverse_loans_trans = log(reverse_loans)

lambda = powerTransform(income_assets, family="yjPower")$roundlam
bankdata$income_assets_trans = yjPower(income_assets, lambda)

lambda = powerTransform(income_equity, family="yjPower")$roundlam
bankdata$income_equity_trans = yjPower(income_equity, lambda)

lambda = powerTransform(income_loans, family="yjPower")$roundlam
bankdata$income_loans_trans = yjPower(income_loans, lambda)
lambda = powerTransform(cf_loans, family="yjPower")$roundlam
bankdata$cf_loans_trans = yjPower(cf_loans, lambda)

bankglm = glm(default ~ assets_tot + assetMcash_tot + assets_loans + reverse_loans_trans + income_assets_trans + income_equity_trans
              + income_loans_trans + cost_sales+ cf_loans_trans, family=binomial,data=bankdata)
predict(bankglm, type="response")
detach(bankdata)
