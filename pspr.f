	function fermat(p,base)
	implicit integer*16(a-z)
	fermat=powm(base,p-1,p)
	return
	end

	function powm(a,b,m)
	implicit integer*16(a-z)
	dimension binb(128)
	do 1 i=1,128
	binb(i)=mod(b/2**(i-1),2)
1	continue
	w=1
	do 2 i=128,1,-1
	w=mulm(w,w,m)
	if(binb(i).lt.1) goto 3
	w=mulm(w,a,m)
3	continue
2	continue
	powm=w
	return
	end

	function mulm(a,b,m)
	implicit integer*16(a-z)
	dimension binb(128)
	do 1 i=1,128
	binb(i)=mod(b/2**(i-1),2)
1	continue
	w=1
	do 2 i=128,1,-1
	w=addm(w,w,m)
	if(binb(i).lt.1) goto 3
	w=addm(w,a,m)
3	continue
2	continue
	mulm=w
	return
	end

	function addm(a,b,m)
	implicit integer*16(a-z)
	if(b.ge.m-a) goto 1
	addm=a+b
	return
1	continue
	addm=a-(m-b)
	return
	end

	function examp(p)
	implicit integer*16(a-z)
	base=2
	if(fermat(p,base).ne.1) goto 1
	base=3
	if(fermat(p,base).ne.1) goto 1
	base=5
	if(fermat(p,base).ne.1) goto 1
	if(pr(p).eq.1) goto 1
	examp=1
	return
1	continue
	examp=0
	return
	end

	implicit integer*16(a-z)
	read *,c
	do 1 p=7,10**c,2
	if (examp(p).eq.0) goto 2
	print *,p
2	continue
1	continue
	print *,'EOF'
	stop
	end

	function pr(p)
	implicit integer*16(a-z)
	k=2
1	continue
	if(k.gt.p/k) goto 2
	if(mod(p,k).eq.0) goto 3
	k=k+1
	goto 1
2	pr=1
	return
3	pr=0
	return
	end
