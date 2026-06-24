	implicit integer*8(a-z)
	read *,mult
	nmin=(mult-1)*2**29
	nmax=mult*2**29
	do 1 n=nmin,nmax,2
	p=3
7	continue
	if(p.gt.n-p) goto 9
	q=n-p
	t=1
10	continue
	if(t.gt.2) goto 1
	q=n-q
	k=3
3	continue
	if(k.gt.q/k) goto 2
	if(mod(q,k).ne.0) goto 5
	goto 6
5	k=k+2
	goto 3
6	if(t.eq.1) goto 8
	q=n-q
	t=1
8	p=q+2
	goto 7
2	continue
	t=t+1
	goto 10
1	continue
	print *,.true.
	stop
9	print *,.false.
	stop
	end
