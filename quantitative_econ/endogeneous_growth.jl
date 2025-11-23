##Goal: replicate the endogeneous growth model of lecture 5

##Date: 18/11/2025

#1. Define the problem 

util = (c,cond) -> cond ? log(c) : -Inf #I wonder to what extend we can change this to the true definition we have in the slides; this is a simplifation.   
R = (k, ls, α, δ)->  ls* k^α + (1.0-δ)*k

#we define the R because we will optimize wrt k' and we will introduce k' in the loop implicitly.


# 2) Define some parameters
α    = 0.33    # Capital elasticity in production function
β    = 0.95    # Discount factor
δ    = 0.06    # Depreciation rate
σ    = 0.2     # elasticity of
param= [α,β,δ,σ]


#transition states

include("Markov_Chain_tools.jl")

ρ    = 0.8     # persistence of the shock
σ    = 0.05    # Volatility of the innovation
μ    = 0       # Average of the process
N    = 5       # Number of points in the grid of shocks
Δ    = 3.0     # number of std deviations away from the mean (Tauchen only)
la_t,P_t = tauchen(N,ρ,σ,μ,Δ)   # If Tauchen's (1986) method is chosen
la_rou,P_rou = rouwenhorst(N,ρ,σ,μ) # If Rouwenhorst's (1995) method is chosen


ls= exp.(la_t)
Ns= 5  ##number of states we are using.
#in the next case I will do it manually, but I understand the idea completely. 

#define grid of values of Kt, our state variable

kss  = (α*β/(1.0-β*(1.0-δ)))^(1.0/(1.0-α)) # Deterministic steady state of capital
Δk   = 0.9
kmin = (1.0-Δk)*kss
kmax = (1.0+Δk)*kss

Nk= 1000

kgrid= range(kmin, kmax, Nk)

#now we define a vector of resources per k, s of the grid. This is the potential resources we may have in the future.

Ri= [R(kgrid[ik],ls[is],α, δ ) for ik in 1:Nk, is in 1:Ns]


## function to find the fixed point. 

using PyPlot, Interpolations

function expectation(x, P)
    x'*P
end 


function t_operator(v, kgrid, param, P, Ri) #This is just a function to find the T operator (opimize right hand side)

α,β,δ,σ = param


Tv= similar(v)
Gk= similar(v)
Ns, Nk= size(v)
Ev= expectation(v, P)

    for is in 1:Ns
        index=1
            for ik in 1:Nk ##loop over 1000
                Vmax=-Inf
                k_prime=0.0
                R= Ri[ik, is]

                for ij in index:Nk
                    kp= kgrid[ij]
                    cond= (R- kp>0) & (kp-(1-δ)kgrid[ik]>0)
                    vtmp= util(R- kp, cond)+ β*Ev[ij, is] ###or ij?
                    if (vtmp>=Vmax)
                        kp= grid[ij]
                        index= ij
                        k_prime= kp
                    else 
                        break
                    end 
                end 

                Tv[ik, is]= Vmax
                Gk[ik, is]= k_prime
            end
    return Tv, Gk
    end 

end 


function bellman_loop(kgrid, agrid, param, P, Ri)
tol= 1e-08
err=1
Nk = length(kgrid)
Na = length(agrid)
v  = zeros(Float64,Nk,Na) 
Gk0= similar(v)

    while err>tol
        Tv, Gk= t_operator(v, kgrid, param, P, Ri)
        err= abs.(Tv-v) : abs.(Gk-Gk0)
        v= copy(Tv)
        Gk0= copy(Gk)
    end 
    return Tv, Gk
end 

##Run this bullshit

Tv, Gk= bellman_loop(ls, kgrid,param,P_t, Ri )

