from pyswip import Functor, Variable, Query

# atualiza a lista de células visitadas pelo jogador consultando o prolog
def update_visited(prolog, visited_list):
    var_x, var_y = Variable(), Variable()
    functor_visited = Functor("visitado", 2)
    query = Query(functor_visited(var_x, var_y))
    visited_list.clear()
    while query.nextSolution():
        visited_list.append((var_x.value, var_y.value))
    query.closeQuery()

# atualiza a lista de células consideradas seguras pelo jogador consultando o prolog
def update_certain(prolog, certain_list):
    var_x, var_y = Variable(), Variable()
    functor_certain = Functor("certeza", 2)
    query = Query(functor_certain(var_x, var_y))
    certain_list.clear()
    while query.nextSolution():
        certain_list.append((var_x.value, var_y.value))
    query.closeQuery()

# atualiza o mapa do jogador com todas as informações conhecidas consultando o prolog
def update_player_map(prolog, map_grid):
    var_x, var_y, var_tile = Variable(), Variable(), Variable()
    functor_tile = Functor("tile", 3)
    query = Query(functor_tile(var_x, var_y, var_tile))
    while query.nextSolution():
        map_grid[var_y.get_value() - 1][var_x.get_value() - 1] = str(var_tile.value)
    query.closeQuery()

# atualiza o mapa do jogador com as percepções atuais (o que ele sente em cada célula)
def update_player_map_perceived(prolog, map_grid):
    for i in range(len(map_grid)):
        for j in range(len(map_grid[0])):
            map_grid[i][j] = ""
    var_x, var_y, var_mem = Variable(), Variable(), Variable()
    functor_memory = Functor("memory", 3)
    query = Query(functor_memory(var_x, var_y, var_mem))
    while query.nextSolution():
        row_idx = var_y.get_value() - 1
        col_idx = var_x.get_value() - 1
        for symbol in var_mem.value:
            symbol_str = str(symbol)
            # adiciona símbolos para cada percepção encontrada
            if symbol_str == "brisa":
                map_grid[row_idx][col_idx] += "P"
            elif symbol_str == "palmas":
                map_grid[row_idx][col_idx] += "T"
            elif symbol_str == "passos":
                map_grid[row_idx][col_idx] += "D"
            elif symbol_str == "reflexo":
                map_grid[row_idx][col_idx] += "U"
            elif symbol_str == "brilho":
                map_grid[row_idx][col_idx] += "O"
    query.closeQuery()

# retorna a posição e direção atual do jogador consultando o prolog
def update_player_pos(prolog):
    pos_x, pos_y, pos_dir = Variable(), Variable(), Variable()
    functor_pos = Functor("posicao", 3)
    query = Query(functor_pos(pos_x, pos_y, pos_dir))
    query.nextSolution()
    result = (pos_x.value, pos_y.value, str(pos_dir.value))
    query.closeQuery()
    return result

# retorna a energia atual do jogador consultando o prolog
def update_energy(prolog):
    var_energy = Variable()
    functor_energy = Functor("energia", 1)
    query = Query(functor_energy(var_energy))
    query.nextSolution()
    energy_val = var_energy.value
    query.closeQuery()
    return energy_val

# retorna a pontuação atual do jogador consultando o prolog
def update_score(prolog):
    var_score = Variable()
    functor_score = Functor("pontuacao", 1)
    query = Query(functor_score(var_score))
    query.nextSolution()
    score_val = var_score.value
    query.closeQuery()
    return score_val

# retorna a quantidade de ouro do jogador consultando o prolog
def update_gold(prolog):
    var_gold = Variable()
    functor_gold = Functor("ouro", 1)
    query = Query(functor_gold(var_gold))
    query.nextSolution()
    gold_val = var_gold.value
    query.closeQuery()
    return gold_val

# marca no mapa as células seguras conhecidas pelo jogador
def get_map_perceived_detailed(prolog, map_grid):
    var_x, var_y = Variable(), Variable()
    functor_safe = Functor("seguro", 2)
    query = Query(functor_safe(var_x, var_y))
    while query.nextSolution():
        row_idx = var_y.value - 1
        col_idx = var_x.value - 1
        map_grid[row_idx][col_idx] = "."
    query.closeQuery()
    return map_grid