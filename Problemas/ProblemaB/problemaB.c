#include <stdio.h>
#include <stdlib.h>

int cavaleiro(int **ocupadas, int n, int x, int y); //retorna 1 se for possível ou 0 se for impossível
int **lerOcupadas(int **a, int n);
int possivelMover(int **ocupadas, int n, int x, int y); /*Retorna 1 se for possível mover ou 0 se for impossível*/
int tudoOcupado(int **ocupadas, int n);
int numSaltos(int x, int y, int **ocupadas, int n);
void posOrdenadas(int x, int y, int xPos[8], int ypos[8], int **ocupadas, int n);

int main(int argc, char const *argv[])
{
    int n, x, y;
    int **tabuleiro;

    scanf(" %d %d %d", &n, &x, &y);

    tabuleiro = (int **)malloc(n * sizeof(int *));
    for (int i = 0; i < n; i++)
    {
        tabuleiro[i] = (int *)calloc(n, n * sizeof(int));
    }
    tabuleiro[x][y] = 1;
    tabuleiro = lerOcupadas(tabuleiro, n);

    if (cavaleiro(tabuleiro, n, x, y))
    {
        printf("YES\n");
    }
    else
    {
        printf("NO\n");
    }

    return 0;
}

int **lerOcupadas(int **a, int n)
{
    int k, x, y;

    scanf(" %d", &k);
    for (int i = 0; i < k; i++)
    {
        scanf(" %d %d", &x, &y);
        a[x][y] = 1;
    }

    return a;
}

int cavaleiro(int **ocupadas, int n, int x, int y)
{
    int xPos[8] = {2, 1, -2, -1, 2, 1, -2, -1};
    int yPos[8] = {1, 2, -1, -2, -1, -2, 1, 2};

    posOrdenadas(x, y, xPos, yPos, ocupadas, n);

    if (tudoOcupado(ocupadas, n))
    {
        return 1;
    }

    for (int i = 0; i < 8; i++)
    {
        if (possivelMover(ocupadas, n, x + xPos[i], y + yPos[i]))
        {
            printf("(%d, %d) -> (%d, %d)\n", x, y, x + xPos[i], y + yPos[i]);
            ocupadas[x + xPos[i]][y + yPos[i]] = 1;
            if (cavaleiro(ocupadas, n, x + xPos[i], y + yPos[i]))
            {
                return 1;
            }
            else
            {
                printf("(%d, %d) <- (%d, %d)\n", x, y, x + xPos[i], y + yPos[i]);
                ocupadas[x + xPos[i]][y + yPos[i]] = 0;
            }
        }
    }
    return 0;
}

int possivelMover(int **ocupadas, int n, int x, int y)
{
    if (x >= 0 && y >= 0 && x < n && y < n && !ocupadas[x][y])
    {
        return 1;
    }
    return 0;
}

int tudoOcupado(int **ocupadas, int n)
{

    for (int i = 0; i < n; i++)
    {

        for (int j = 0; j < n; j++)
        {
            if (!ocupadas[i][j])
                return 0;
        }
    }
    return 1;
}

void posOrdenadas(int x, int y, int xPos[8], int yPos[8], int **ocupadas, int n)
{
    int flag, auxX, auxY;

    //Fazer algoritmo de ordenação melhor
    do
    {
        flag = 0;
        for (int i = 0; i < 7; i++)
        {
            if (numSaltos(x + xPos[i], y + yPos[i], ocupadas, n) > numSaltos(x + xPos[i + 1], y + yPos[i + 1], ocupadas, n))
            {
                auxX = xPos[i];
                xPos[i] = xPos[i + 1];
                xPos[i + 1] = auxX;
                auxY = yPos[i];
                yPos[i] = yPos[i + 1];
                yPos[i + 1] = auxY;
                flag++;
            }
        }
    } while (flag != 0);
}

int numSaltos(int x, int y, int **ocupadas, int n)
{
    int saltos = 0;
    int xPos[8] = {2, 1, -2, -1, 2, 1, -2, -1};
    int yPos[8] = {1, 2, -1, -2, -1, -2, 1, 2};

    if (!possivelMover(ocupadas, n, x, y))
        return 10;

    for (int i = 0; i < 8; i++)
    {
        if (possivelMover(ocupadas, n, x + xPos[i], y + yPos[i]))
        {
            saltos++;
        }
    }

    return saltos;
}