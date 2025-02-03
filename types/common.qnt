module types::common {
    pure val zeroAddress = 0
    pure val maxUint = 2^256 - 1
    pure val decimalShannon = 9
    pure val decimalSzabo = 6
    pure val decimalFinney = 3

    type Address = int
    type Uint = int

    pure def isUint(i: int): bool = 
        0 <= i and i <= maxUint
    
    type Result[a] = { ok: a, err: str }
    
    pure def success(value: a): Result[a] = { 
        ok: value,
        err: "",
    }

    pure def failure(
        value: a,
        reason: str,
    ): Result[a] = { 
        ok: value,
        err: reason,
    }

    pure def isOk(r: Result[a]): bool = r.err == ""
    pure def isErr(r: Result[a]): bool = r.err != ""

    pure def require(cond: bool, msg: str): str =
        if (cond) "" else msg
    
    pure def andRequire(prevErr: str, cond: bool, msg: str): str = 
        if (prevErr != "") prevErr else require(cond, msg)
    
    pure def getOrElse(m: a -> b, key: a, default: b): b =
        if (key.in(m.keys())) m.get(key) else default

    pure def safeSetBy(m: a -> b, key: a, default: b, fn: (b) => b): a -> b =
        if (key.in(m.keys())) {
            m.setBy(key, (v) => fn(v)) 
        } else{
            m.put(key, default).setBy(key, (v) => fn(v))
        }

    pure def shannon(n: Uint): Uint =
        n * 10.ipow(decimalShannon)

    pure def szabo(n: Uint): Uint =
        n * 10.ipow(decimalSzabo)

    pure def finney(n: Uint): Uint =
        n * 10.ipow(decimalFinney)

    pure def bounds(n: Uint): Set[Uint] =
        Set(0, 1, n/2, n, n+1)
}
