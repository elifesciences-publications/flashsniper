function blk_stddev,array,w
		z = smooth(array,w,edge = 1)
		zz = smooth(array^2,w,edge=1)
		ans = sqrt(zz-z^2)
		return,ans
end