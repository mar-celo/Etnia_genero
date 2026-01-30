# Gerador de Gráficos - Etnia e Gênero

Este repositório contém um script Python para gerar gráficos automaticamente a partir das tabelas de dados na pasta `data/`.

## 📊 Descrição

O script `gerar_graficos.py` analisa os dados do arquivo CSV `data/funcoes_infografico.csv` e gera automaticamente diversos gráficos e um relatório em texto com estatísticas.

## 🚀 Como Usar

### Pré-requisitos

Certifique-se de ter Python 3 instalado e as bibliotecas necessárias:

```bash
pip install pandas matplotlib seaborn
```

### Executar o Script

Para gerar os gráficos, execute:

```bash
python3 gerar_graficos.py
```

## 📈 Gráficos Gerados

O script gera automaticamente 5 gráficos na pasta `graficos/`:

1. **01_distribuicao_agrupamento_geral.png** - Gráfico de barras mostrando a distribuição por Agrupamento Geral
2. **02_distribuicao_tipo_funcao.png** - Gráfico de barras horizontais com os Top 15 Tipos de Função
3. **03_distribuicao_subnivel.png** - Gráfico de barras com os Top 20 Subníveis de Função
4. **04_pizza_agrupamento_decreto.png** - Gráfico de pizza da Distribuição por Agrupamento Decreto
5. **05_resumo_geral.png** - Painel com múltiplos gráficos e estatísticas resumidas

Além disso, é gerado um arquivo de texto:

6. **relatorio.txt** - Relatório detalhado com estatísticas e distribuições

## 📁 Estrutura de Pastas

```
Etnia_genero/
├── data/
│   └── funcoes_infografico.csv    # Dados de entrada
├── graficos/                       # Gráficos gerados (criado automaticamente)
│   ├── 01_distribuicao_agrupamento_geral.png
│   ├── 02_distribuicao_tipo_funcao.png
│   ├── 03_distribuicao_subnivel.png
│   ├── 04_pizza_agrupamento_decreto.png
│   ├── 05_resumo_geral.png
│   └── relatorio.txt
└── gerar_graficos.py              # Script principal
```

## 📊 Dados Analisados

O script analisa os seguintes aspectos dos dados:

- **Agrupamento Geral**: Categorização geral das funções
- **Tipo de Função Detalhada**: Tipos específicos de funções
- **Subnível de Função**: Subníveis dentro de cada função
- **Agrupamento Decreto**: Agrupamento por decreto

## ⚙️ Personalização

Para personalizar os gráficos, você pode editar o arquivo `gerar_graficos.py` e ajustar:

- Cores dos gráficos
- Tamanho das figuras
- Número de itens exibidos nos tops (atualmente Top 15 e Top 20)
- Estilo dos gráficos (seaborn styles)
- Resolução (DPI) dos gráficos

## 📝 Observações

- Os gráficos são salvos em alta resolução (300 DPI) no formato PNG
- O script cria automaticamente a pasta `graficos/` se ela não existir
- Todos os gráficos e o relatório são recriados a cada execução do script
- O relatório em texto (`relatorio.txt`) fornece estatísticas detalhadas complementares aos gráficos

## 🔧 Solução de Problemas

### Erro ao carregar dados
Certifique-se de que o arquivo `data/funcoes_infografico.csv` existe e está no formato correto.

### Bibliotecas não encontradas
Instale as dependências necessárias:
```bash
pip install pandas matplotlib seaborn
```

### Permissão negada
Torne o script executável:
```bash
chmod +x gerar_graficos.py
```

## 📄 Licença

Este projeto faz parte do repositório Etnia_genero.
