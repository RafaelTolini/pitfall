from queue import PriorityQueue

# prepara o grid para o algoritmo de caminho, marcando células livres
def grid_livre(map_data, get_map_perceived_detailed):
    grid = [linha[:] for linha in map_data]
    grid = get_map_perceived_detailed(grid)
    for row_idx in range(len(grid)):
        for col_idx in range(len(grid[0])):
            if grid[row_idx][col_idx] in ("O", "U"):
                grid[row_idx][col_idx] = "."
    return grid

# gera os movimentos possíveis a partir da posição e direção atual
def get_neighbours(grid, pos_x, pos_y, dir_idx, DELTA, LEFT, RIGHT):
    yield (pos_x, pos_y, LEFT(dir_idx)), 1, "virar_esquerda"   # virar à esquerda
    yield (pos_x, pos_y, RIGHT(dir_idx)), 1, "virar_direita"   # virar à direita
    delta_x, delta_y = DELTA[dir_idx]
    next_x, next_y = pos_x + delta_x, pos_y + delta_y
    # anda para frente se a célula for livre
    if 0 <= next_x < 12 and 0 <= next_y < 12 and grid[next_y][next_x] == ".":
        yield (next_x, next_y, dir_idx), 1, "andar"

# algoritmo principal de busca a* para encontrar o melhor caminho
def A_star(grid, start_state, goal_state, DELTA, LEFT, RIGHT):
    cost_map = {start_state: 0}
    prev_node, action_map = {start_state: None}, {start_state: None}
    pq = PriorityQueue()
    heuristic = abs(start_state[0] - goal_state[0]) + abs(start_state[1] - goal_state[1])
    pq.put((heuristic, start_state))

    while not pq.empty():
        _, current = pq.get()
        cur_x, cur_y, cur_dir = current
        # verifica se chegou no objetivo
        if (cur_x, cur_y) == goal_state:
            return cost_map, prev_node, action_map
        # tenta todos os movimentos possíveis
        for next_state, step_cost, action in get_neighbours(grid, cur_x, cur_y, cur_dir, DELTA, LEFT, RIGHT):
            total_cost = cost_map[current] + step_cost
            if total_cost < cost_map.get(next_state, 1e9):
                cost_map[next_state] = total_cost
                prev_node[next_state], action_map[next_state] = current, action
                heuristic = abs(next_state[0] - goal_state[0]) + abs(next_state[1] - goal_state[1])
                pq.put((total_cost + heuristic, next_state))
    # se não encontrou caminho
    return None, None, None

# reconstrói o caminho de ações a partir do resultado da busca
def extrai_caminho(cost_map, prev_node, action_map, goal_state):
    end_state = min(
        ((goal_state[0], goal_state[1], d) for d in range(4) if (goal_state[0], goal_state[1], d) in cost_map),
        key=lambda state: cost_map[state],
    )
    path = []
    node = end_state
    while action_map[node]:
        path.append(action_map[node])
        node = prev_node[node]
    return list(reversed(path))