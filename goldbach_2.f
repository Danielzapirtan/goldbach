	function fastpr(p)
	implicit integer*16(a-z)
	k=3
1	continue
	if(k.gt.p/k) goto 2
	if(mod(p,k).eq.0) goto 3
	k=k+2
	goto 1
2	continue
	fastpr=1
	return
3	continue
	fastpr=0
	return
	end

	subroutine inittab(tabpr)
	implicit integer*16(a-z)
	dimension tabpr(1)
	p=3
	ix=1
	tabpr(ix)=2
	ix=2
1	continue
	if(fastpr(p).eq.0) goto 2
	tabpr(ix)=p
	ix=ix+1
2	p=p+2
	if(.not.(p.gt.262144)) goto 1
	tabpr(ix)=0
	return
	end

	function pr(p, tabpr)
	implicit integer*16(a-z)
	dimension tabpr(1)
	ix=2
1	continue
	k=tabpr(ix)
	if(k.eq.0) goto 2
	if(k.gt.p/k) goto 2
	if(mod(p,k).eq.0) goto 3
	ix=ix+1
	goto 1
2	continue
	pr=1
	return
3	continue
	pr=0
	return
	end

	function gbn(n,tabpr)
	implicit integer*16(a-z)
	dimension tabpr(1)
	do 1 p=3,n/2,2
	if(pr(p,tabpr).eq.0) goto 1
	if(pr(n-p,tabpr).eq.0) goto 1
	gbn=1
	return
1	continue
	gbn=0
	return
	end

	function gbnab(a,b,tabpr)
	implicit integer*16(a-z)
	dimension tabpr(1)
	do 1 n=a,b,2
	if(gbn(n,tabpr).eq.1) goto 1
	gbnab=0
	return
1	continue
	gbnab=1
	return
	end

	implicit integer*16(a-z)
	dimension tabpr(32768)
	logical exitst
	read *,c
	call inittab(tabpr)
	slice=2**27
	a=(c-1)*slice
	b=c*slice
	exitst=gbnab(a,b,tabpr).eq.1
	print *,exitst
	stop
	end
