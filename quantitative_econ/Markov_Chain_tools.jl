using LinearAlgebra, Interpolations, Distributions, FastGaussQuadrature
Φ(x::T) where {T<:Real} = cdf(Normal(0, 1), x)


function tauchen(N::Integer,ρ::T1,σ::T2,μ = zero(promote_type(T1, T2)),n_std::Real = 3.0) where {T1<:Real,T2<:Real}
    """
    tauchen(N::Integer,ρ::T1,σ::T2,μ = zero(promote_type(T1, T2)),n_std::Real = 3.0) 
    
    Discretize a process of the form x_t=ρ x_{t-1}+(1-ρ)*μ+ϵ_t
    """
    # Get discretized space
    a_bar = n_std * sqrt(σ^2 / (1 - ρ^2))
    y = LinRange(-a_bar, a_bar, N)
    d = y[2] - y[1]

    # Get transition probabilities
    P = zeros(promote_type(T1, T2), N, N)
    @inbounds for i = 1:N
        # Do end points first
        P[i, 1] = Φ((y[1] - ρ * y[i] + d / 2) / σ)
        P[i, N] = 1 - Φ((y[N] - ρ * y[i] - d / 2) / σ)

        # fill in the middle columns
        for j = 2:N-1
            P[i, j] = (Φ((y[j] - ρ * y[i] + d / 2) / σ) -
                       Φ((y[j] - ρ * y[i] - d / 2) / σ))
        end
    end
    Y = collect(y) .+ μ
    P = P ./ sum(P, dims = 2)

    return Y, P
end

function rouwenhorst(N::Integer,ρ::T1,σ::T2,μ = zero(promote_type(T1, T2))) where {T1<:Real,T2<:Real}
    σ_y = σ / sqrt(1-ρ^2)
    p   = (1+ρ)/2
    Θ   = [p 1-p; 1-p p]
    Δ   = sqrt(N-1) * σ_y
    Y,P = _rouwenhorst(p, p, μ, Δ, N)
    return Y,P
end

function _rouwenhorst(p::Real, q::Real, μ::Real, Δ::Real, N::Integer)
    if N == 2
        return [μ-Δ,μ+Δ],[p 1-p;1-q q]
    else
        _, P_nm1       = _rouwenhorst(p,q,μ,Δ,N-1)
        P              = p*[P_nm1 zeros(N-1,1); zeros(1,N)]+
                         (1-p)*[zeros(N-1,1) P_nm1; zeros(1,N)] +
                         q*[zeros(1,N); zeros(N-1,1) P_nm1] +
                         (1-q)*[zeros(1,N); P_nm1 zeros(N-1,1)]
        P[2:end-1,:] ./= 2
        Y              = collect(range(μ-Δ, stop=μ+Δ, length=N))
        return Y,P
    end
end

function tauchen_hussey(N::Integer,ρ::T1,σ::T2,μ = zero(promote_type(T1, T2))) where {T1<:Real,T2<:Real}
    xx,wx = gausshermite(N)
    Y     = sqrt(2.0)*σ*xx.+μ
    x     = repeat(xx,1,N)
    y     = x'
    w     = repeat(wx,1,N)
    # 
    # Computation
    # 
    P     = (exp.(y.*y-(y-ρ*x).*(y-ρ*x)).*w)./sqrt(π)
    sx    = repeat(sum(P,dims=2),1,N)
    P     = P./sx
    return  Y,P
end

function stationnary_distribution(P::Array{Float64,2})
    N    = size(P,1)
    D    = ones(N)/N
    tol  = 1e-12
    crit = 1.0
    while crit>tol
        D0   = P'*D
        crit = norm(D0-D,Inf)
        D    = copy(D0)
    end
    return D
end
function  simulate_markov(z::Array{Float64,1},P::Array{Float64,2},T::Int64;seed=[],init_state=0)
    if !isempty(seed)
        Random.seed!(seed)
    end
    chain::Array{Int64,1}=[]
    st=1
    if (init_state<=0)
        D=stationnary_distribution(P)
        CD=cumsum(D)
        e=rand()   
        st=findfirst(e.<=CD)
    else
        st=init_state
    end
    push!(chain,st)
    CP=cumsum(P,dims=2)
    for t in 2:T
        e=rand() 
        st=findfirst(e.<=CP[st,:])
        push!(chain,st)
    end
    return chain,z[chain]
end

function  simulate_markov(z::Array{Float64,2},P::Array{Float64,2},T::Int64;seed=[],init_state=0)
    if !isempty(seed)
        Random.seed!(seed)
    end
    chain::Array{Int64,1}=[]
    st=1
    if (init_state<=0)
        D=stationnary_distribution(P)
        CD=cumsum(D)
        e=rand()   
        st=findfirst(e.<=CD)
    else
        st=init_state
    end
    push!(chain,st)
    CP=cumsum(P,dims=2)
    for t in 2:T
        e=rand() 
        st=findfirst(e.<=CP[st,:])
        push!(chain,st)
    end
    return chain,z[chain,:]
end