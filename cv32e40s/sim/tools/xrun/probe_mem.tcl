database -open waves -default -incsize 2G
probe -create uvmt_cv32e40s_tb -depth all -all -memories -database waves

run
exit
