aTable = {}

for i = 1, 10 do
    aTable[i] = i
end

--print("first num:", aTable[1])

--print(#aTable)

cientistas = {
    clattes = {
        nome = "Cesar",
        sobrenome = "Lattes",
        ano_nascimento = 1924,
        ano_falecimento = 2005,
        area = "fisica",
        contribuicao = "meson pi"
    },
    jpalis = {
        nome = "Jacob",
        sobrenome = "Palis",
        ano_nascimento = 1940,
        ano_falecimento = 0000,
        contribuicao = "sistemas dinamicos"
    }
}

function getCientist(cientist)
    for k, v in pairs(cientistas) do  
        if k==cientist then
            return v.nome, v.sobrenome, v.ano_falecimento
        end
    end
end

a, b,c = getCientist("clattes")
--print(a,b,c)

-- producer-consumer

function receive(prod)
    local status, value = coroutine.resume(prod)
    return value
end

function send(x)
    coroutine.yield(x)
end

function producer()
    return coroutine.create(function()
        while true do
            local x = io.read() -- produce new value
            send(x)
        end
    end)
end

function filter(prod)
    return coroutine.create(function()
        for line = 1, math.huge do
            local x = receive(prod) -- get new value
            x = string.format("%5d %s", line, x)
            send(x) -- send it to consumer
        end
    end)
end

function consumer(prod)
    while true do
        local x = receive(prod) -- get new value
        io.write(x, "\n")       -- consume new value
    end
end

consumer(filter(producer()))


