import pygame
from pyswip import Prolog, Functor, Variable, Query
import pathlib
from queue import PriorityQueue
from astar import *
from player_state import *

current_path = str(pathlib.Path().resolve())

elapsed_time = 0
elapsed_time_div = 50
auto_play_tempo = 0.5
auto_play = True
show_map = False

scale = 60
size_x = 12
size_y = 12
width = size_x * scale
height = size_y * scale

pl_file = "main.pl"
prolog = Prolog()
prolog.consult(pl_file)

last_action = ""
visitados = []
certezas = []
mapa = [
    ["", "", "", "", "", "", "", "", "", "", "", ""],
    ["", "", "", "", "", "", "", "", "", "", "", ""],
    ["", "", "", "", "", "", "", "", "", "", "", ""],
    ["", "", "", "", "", "", "", "", "", "", "", ""],
    ["", "", "", "", "", "", "", "", "", "", "", ""],
    ["", "", "", "", "", "", "", "", "", "", "", ""],
    ["", "", "", "", "", "", "", "", "", "", "", ""],
    ["", "", "", "", "", "", "", "", "", "", "", ""],
    ["", "", "", "", "", "", "", "", "", "", "", ""],
    ["", "", "", "", "", "", "", "", "", "", "", ""],
    ["", "", "", "", "", "", "", "", "", "", "", ""],
    ["", "", "", "", "", "", "", "", "", "", "", ""],
]

player_pos = (1, 1, "norte")
energia = 0
pontuacao = 0
qtd_ouro = 0

DIRS = ["norte", "leste", "sul", "oeste"]
DELTA = [(0, 1), (1, 0), (0, -1), (-1, 0)]
LEFT = lambda d: (d + 3) % 4
RIGHT = lambda d: (d + 1) % 4

pending_actions = []

enemy2_coords = set()

# função para ler coordenadas do inimigo2 do arquivo do mapa
def parse_map_file_for_enemy2(map_file_path):
    global enemy2_coords
    enemy2_coords.clear()
    with open(map_file_path, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if line.startswith("tile(") and "'d'" in line:
                parts = line.split("(")[1].split(")")[0].split(",")
                x = int(parts[0].strip())
                y = int(parts[1].strip())
                enemy2_coords.add((x, y))

parse_map_file_for_enemy2("mapas/mapa_facil.pl")

# função de decisão: consulta a próxima ação no prolog
def decisao():
    acao = ""

    acoes = list(prolog.query("executa_acao(X)"))
    if len(acoes) > 0:
        acao = acoes[0]["X"]

    return acao

# executa ação no prolog
def exec_prolog(a):
    global last_action

    print("Decisão do prolog: " + a)

    if isinstance(a, str) and a.startswith("go_to"):
        inside = a[a.find("(") + 1 : a.rfind(")")]
        x_str, y_str = inside.split(",")
        coords = [int(x_str), int(y_str)]
        go_to(coords)

    elif a != "":
        list(prolog.query(a))

    last_action = a

# atualiza informações do prolog e do estado do jogador
def update_prolog():
    list(prolog.query("atualiza_obs, verifica_player"))

    update_visited(prolog, visitados)
    update_certain(prolog, certezas)

    if show_map:
        update_player_map(prolog, mapa)
    else:
        update_player_map_perceived(prolog, mapa)

    global player_pos, energia, pontuacao, qtd_ouro
    player_pos = update_player_pos(prolog)
    energia = update_energy(prolog)
    pontuacao = update_score(prolog)
    qtd_ouro = update_gold(prolog)

    print("mapa para visualização a*: ")
    detailed = get_map_perceived_detailed(prolog, mapa)
    for row in detailed:
        print(" ".join(cell if cell != "" else "?" for cell in row))
    print()

    print("posição do jogador:")
    print(player_pos)

    print("quantidade de ouro atual:")
    print(qtd_ouro)

    print()

# executa caminho até o destino usando a*
def go_to(target_xy):
    grid = grid_livre(mapa, lambda m: get_map_perceived_detailed(prolog, m))

    sx, sy, sdir = player_pos
    start = (sx - 1, sy - 1, DIRS.index(sdir))
    goal = (target_xy[0] - 1, target_xy[1] - 1)

    g, prev, act = A_star(grid, start, goal, DELTA, LEFT, RIGHT)

    if g is not None:
        list(prolog.query("clear_blocked"))
        for step in extrai_caminho(g, prev, act, goal):
            pending_actions.append(step)
        return

    print("go_to: sem caminho, marcando bloqueado")
    list(prolog.query(f"add_blocked({target_xy[0]},{target_xy[1]})"))

# carrega imagens e fontes do jogo
def load():
    global sys_font, clock, img_wall, img_grass, img_start, img_finish, img_path
    global img_gold, img_health, img_pit, img_bat, img_enemy1, img_enemy2, img_floor
    global bw_img_gold, bw_img_health, bw_img_pit, bw_img_bat, bw_img_enemy1, bw_img_enemy2, bw_img_floor
    global img_player_up, img_player_down, img_player_left, img_player_right, img_tomb

    sys_font = pygame.font.Font(pygame.font.get_default_font(), 20)
    clock = pygame.time.Clock()

    img_wall = pygame.image.load((current_path + "/images/wall.jpg"))
    img_wall_size = (width / size_x, height / size_y)

    img_wall = pygame.transform.scale(img_wall, img_wall_size)

    img_player_up = pygame.image.load(
        (current_path + "/images/player_up.png")
    )
    img_player_up_size = (width / size_x, height / size_y)
    img_player_up = pygame.transform.scale(img_player_up, img_player_up_size)

    img_player_down = pygame.image.load(
        (current_path + "/images/player_down.png")
    )
    img_player_down_size = (width / size_x, height / size_y)
    img_player_down = pygame.transform.scale(img_player_down, img_player_down_size)

    img_player_left = pygame.image.load(
        (current_path + "/images/player_left.png")
    )
    img_player_left_size = (width / size_x, height / size_y)
    img_player_left = pygame.transform.scale(img_player_left, img_player_left_size)

    img_player_right = pygame.image.load(
        (current_path + "/images/player_right.png")
    )
    img_player_right_size = (width / size_x, height / size_y)
    img_player_right = pygame.transform.scale(img_player_right, img_player_right_size)

    img_tomb = pygame.image.load(
        (current_path + "/images/tombstone.png")
    )
    img_tomb_size = (width / size_x, height / size_y)
    img_tomb = pygame.transform.scale(img_tomb, img_tomb_size)

    img_grass = pygame.image.load((current_path + "/images/grass.jpg"))
    img_grass_size = (width / size_x, height / size_y)
    img_grass = pygame.transform.scale(img_grass, img_grass_size)

    img_floor = pygame.image.load((current_path + "/images/floor.png"))
    img_floor_size = (width / size_x, height / size_y)
    img_floor = pygame.transform.scale(img_floor, img_floor_size)

    img_gold = pygame.image.load((current_path + "/images/gold.png"))
    img_gold_size = (width / size_x, height / size_y)
    img_gold = pygame.transform.scale(img_gold, img_gold_size)

    img_pit = pygame.image.load((current_path + "/images/pit.png"))
    img_pit_size = (width / size_x, height / size_y)
    img_pit = pygame.transform.scale(img_pit, img_pit_size)

    img_enemy1 = pygame.image.load((current_path + "/images/enemy1.png"))
    img_enemy1_size = (width / size_x, height / size_y)
    img_enemy1 = pygame.transform.scale(img_enemy1, img_enemy1_size)

    img_enemy2 = pygame.image.load((current_path + "/images/enemy2.png"))
    img_enemy2_size = (width / size_x, height / size_y)
    img_enemy2 = pygame.transform.scale(img_enemy2, img_enemy2_size)

    img_bat = pygame.image.load((current_path + "/images/bat.png"))
    img_bat_size = (width / size_x, height / size_y)
    img_bat = pygame.transform.scale(img_bat, img_bat_size)

    img_health = pygame.image.load((current_path + "/images/health.png"))
    img_health_size = (width / size_x, height / size_y)
    img_health = pygame.transform.scale(img_health, img_health_size)

    bw_img_floor = pygame.image.load(
        (current_path + "/images/bw_floor.png")
    )
    bw_img_floor_size = (width / size_x, height / size_y)
    bw_img_floor = pygame.transform.scale(bw_img_floor, bw_img_floor_size)

    bw_img_gold = pygame.image.load((current_path + "/images/bw_gold.png"))
    bw_img_gold_size = (width / size_x, height / size_y)
    bw_img_gold = pygame.transform.scale(bw_img_gold, bw_img_gold_size)

    bw_img_pit = pygame.image.load((current_path + "/images/bw_pit.png"))
    bw_img_pit_size = (width / size_x, height / size_y)
    bw_img_pit = pygame.transform.scale(bw_img_pit, bw_img_pit_size)

    bw_img_enemy1 = pygame.image.load(
        (current_path + "/images/bw_enemy1.png")
    )
    bw_img_enemy1_size = (width / size_x, height / size_y)
    bw_img_enemy1 = pygame.transform.scale(bw_img_enemy1, bw_img_enemy1_size)

    bw_img_enemy2 = pygame.image.load(
        (current_path + "/images/bw_enemy2.png")
    )
    bw_img_enemy2_size = (width / size_x, height / size_y)
    bw_img_enemy2 = pygame.transform.scale(bw_img_enemy2, bw_img_enemy2_size)

    bw_img_bat = pygame.image.load((current_path + "/images/bw_bat.png"))
    bw_img_bat_size = (width / size_x, height / size_y)
    bw_img_bat = pygame.transform.scale(bw_img_bat, bw_img_bat_size)

    bw_img_health = pygame.image.load(
        (current_path + "/images/bw_health.png")
    )
    bw_img_health_size = (width / size_x, height / size_y)
    bw_img_health = pygame.transform.scale(bw_img_health, bw_img_health_size)


# atualiza o estado do jogo a cada ciclo
def update(dt, screen):
    global elapsed_time

    elapsed_time += dt

    if (elapsed_time / elapsed_time_div) > auto_play_tempo:
        if pending_actions:
            cmd = pending_actions.pop(0)
            exec_prolog(cmd)
            update_prolog()
        else:
            if auto_play and player_pos[2] != "morto":
                cmd = decisao()
                exec_prolog(cmd)
                update_prolog()

        elapsed_time = 0


# trata eventos de teclado e mouse
def key_pressed(event):
    global show_map, elapsed_time_div

    if event.type == pygame.KEYDOWN:

        if not auto_play and player_pos[2] != "morto":
            if event.key == pygame.K_LEFT:
                exec_prolog("virar_esquerda")
                update_prolog()

            elif event.key == pygame.K_RIGHT:
                exec_prolog("virar_direita")
                update_prolog()

            elif event.key == pygame.K_UP:
                exec_prolog("andar")
                update_prolog()

            if event.key == pygame.K_SPACE:
                exec_prolog("pegar")
                update_prolog()

        if event.key == pygame.K_m:
            show_map = not show_map
            update_prolog()

    elif event.type == pygame.MOUSEBUTTONDOWN:
        if event.button == 5:
            elapsed_time_div = min(elapsed_time_div + 50, 10000)
            print(f"elapsed_time_div aumentou para {elapsed_time_div}")
        elif event.button == 4:
            elapsed_time_div = max(elapsed_time_div - 50, 1)
            print(f"elapsed_time_div diminuiu para {elapsed_time_div}")


# desenha o estado atual do jogo na tela
def draw_screen(screen):

    screen.fill((0, 0, 0))

    y = 0
    for j in mapa:
        x = 0
        for i in j:

            if (x + 1, 12 - y) in visitados:
                screen.blit(
                    img_floor, (x * img_floor.get_width(), y * img_floor.get_height())
                )
            else:
                screen.blit(
                    bw_img_floor,
                    (x * bw_img_floor.get_width(), y * bw_img_floor.get_height()),
                )

            if mapa[11 - y][x].find("P") > -1:
                if (x + 1, 12 - y) in certezas:
                    screen.blit(
                        img_pit, (x * img_pit.get_width(), y * img_pit.get_height())
                    )
                else:
                    screen.blit(
                        bw_img_pit,
                        (x * bw_img_pit.get_width(), y * bw_img_pit.get_height()),
                    )

            if mapa[11 - y][x].find("T") > -1:
                if (x + 1, 12 - y) in certezas:
                    screen.blit(
                        img_bat, (x * img_bat.get_width(), y * img_bat.get_height())
                    )
                else:
                    screen.blit(
                        bw_img_bat,
                        (x * bw_img_bat.get_width(), y * bw_img_bat.get_height()),
                    )

            # enemy1 ou enemy2
            if mapa[11 - y][x].find("D") > -1 or mapa[11 - y][x].find("d") > -1:
                coord = (x + 1, 12 - y)
                if coord in enemy2_coords:
                    if coord in certezas:
                        screen.blit(
                            img_enemy2,
                            (x * img_enemy2.get_width(), y * img_enemy2.get_height()),
                        )
                    else:
                        screen.blit(
                            bw_img_enemy2,
                            (x * bw_img_enemy2.get_width(), y * img_enemy2.get_height()),
                        )
                else:
                    if coord in certezas:
                        screen.blit(
                            img_enemy1,
                            (x * img_enemy1.get_width(), y * img_enemy1.get_height()),
                        )
                    else:
                        screen.blit(
                            bw_img_enemy1,
                            (x * bw_img_enemy1.get_width(), y * img_enemy1.get_height()),
                        )

            if mapa[11 - y][x].find("U") > -1:
                if (x + 1, 12 - y) in certezas:
                    screen.blit(
                        img_health,
                        (x * img_health.get_width(), y * img_health.get_height()),
                    )
                else:
                    screen.blit(
                        bw_img_health,
                        (x * bw_img_health.get_width(), y * img_health.get_height()),
                    )

            if mapa[11 - y][x].find("O") > -1:
                if (x + 1, 12 - y) in certezas:
                    screen.blit(
                        img_gold, (x * img_gold.get_width(), y * img_gold.get_height())
                    )
                else:
                    screen.blit(
                        bw_img_gold,
                        (x * bw_img_gold.get_width(), y * img_gold.get_height()),
                    )

            if x == player_pos[0] - 1 and y == 12 - player_pos[1]:
                if player_pos[2] == "norte":
                    screen.blit(
                        img_player_up,
                        (x * img_player_up.get_width(), y * img_player_up.get_height()),
                    )
                elif player_pos[2] == "sul":
                    screen.blit(
                        img_player_down,
                        (
                            x * img_player_down.get_width(),
                            y * img_player_down.get_height(),
                        ),
                    )
                elif player_pos[2] == "leste":
                    screen.blit(
                        img_player_right,
                        (
                            x * img_player_right.get_width(),
                            y * img_player_right.get_height(),
                        ),
                    )
                elif player_pos[2] == "oeste":
                    screen.blit(
                        img_player_left,
                        (
                            x * img_player_left.get_width(),
                            y * img_player_left.get_height(),
                        ),
                    )
                else:
                    screen.blit(
                        img_tomb, (x * img_tomb.get_width(), y * img_tomb.get_height())
                    )
            x += 1
        y += 1

    t = sys_font.render("pontuação: " + str(pontuacao), False, (255, 255, 255))
    screen.blit(t, t.get_rect(top=height + 5, left=10))

    t = sys_font.render(last_action, False, (255, 255, 255))
    screen.blit(t, t.get_rect(top=height + 5, left=width - 500))

    t = sys_font.render("energia: " + str(energia), False, (255, 255, 255))
    screen.blit(t, t.get_rect(top=height + 5, left=width - 300))

    t = sys_font.render("ouro: " + str(qtd_ouro), False, (255, 255, 255))
    screen.blit(t, t.get_rect(top=height + 5, left=width - 150))


# loop principal do jogo
def main_loop(screen):
    global clock
    running = True

    while running:
        for e in pygame.event.get():
            if e.type == pygame.QUIT:
                running = False
                break

            key_pressed(e)

        dt = clock.tick()
        update(dt, screen)
        draw_screen(screen)
        pygame.display.update()

update_prolog()

pygame.init()
pygame.display.set_caption("INF1771 Trabalho 2 - Agente Lógico")
screen = pygame.display.set_mode((width, height + 30))
load()

main_loop(screen)
pygame.quit()