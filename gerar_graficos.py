#!/usr/bin/env python3
"""
Script para gerar gráficos das tabelas na pasta data
Generates charts from tables in the data folder
"""

import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import os
from pathlib import Path

# Configurar estilo dos gráficos
sns.set_style("whitegrid")
plt.rcParams['figure.figsize'] = (12, 8)
plt.rcParams['font.size'] = 10

# Criar pasta para salvar os gráficos
output_dir = Path("graficos")
output_dir.mkdir(exist_ok=True)

print("=" * 60)
print("Gerando gráficos das tabelas na pasta data")
print("=" * 60)

# Carregar dados do CSV
csv_file = "data/funcoes_infografico.csv"
print(f"\nCarregando dados de: {csv_file}")

try:
    df = pd.read_csv(csv_file)
    print(f"✓ Dados carregados com sucesso: {len(df)} registros")
    print(f"✓ Colunas disponíveis: {list(df.columns)}")
except Exception as e:
    print(f"✗ Erro ao carregar dados: {e}")
    exit(1)

# Exibir informações básicas sobre os dados
print("\n" + "=" * 60)
print("Informações dos dados:")
print("=" * 60)
print(df.head())
print("\nEstatísticas descritivas:")
print(df.describe())

# Gráfico 1: Distribuição por Agrupamento Geral
print("\n[1/5] Gerando gráfico de distribuição por Agrupamento Geral...")
try:
    plt.figure(figsize=(12, 6))
    agrupamento_counts = df['Agrupamento Geral'].value_counts()
    agrupamento_counts.plot(kind='bar', color='steelblue')
    plt.title('Distribuição por Agrupamento Geral', fontsize=16, fontweight='bold')
    plt.xlabel('Agrupamento Geral', fontsize=12)
    plt.ylabel('Quantidade', fontsize=12)
    plt.xticks(rotation=45, ha='right')
    plt.tight_layout()
    output_file = output_dir / "01_distribuicao_agrupamento_geral.png"
    plt.savefig(output_file, dpi=300, bbox_inches='tight')
    print(f"✓ Salvo: {output_file}")
    plt.close()
except Exception as e:
    print(f"✗ Erro ao gerar gráfico 1: {e}")

# Gráfico 2: Distribuição por Tipo de Função Detalhada
print("\n[2/5] Gerando gráfico de distribuição por Tipo de Função...")
try:
    plt.figure(figsize=(14, 7))
    tipo_funcao_counts = df['Tipo Função Detalhada2'].value_counts().head(15)
    tipo_funcao_counts.plot(kind='barh', color='coral')
    plt.title('Top 15 Tipos de Função Detalhada', fontsize=16, fontweight='bold')
    plt.xlabel('Quantidade', fontsize=12)
    plt.ylabel('Tipo de Função', fontsize=12)
    plt.tight_layout()
    output_file = output_dir / "02_distribuicao_tipo_funcao.png"
    plt.savefig(output_file, dpi=300, bbox_inches='tight')
    print(f"✓ Salvo: {output_file}")
    plt.close()
except Exception as e:
    print(f"✗ Erro ao gerar gráfico 2: {e}")

# Gráfico 3: Distribuição por Subnível de Função
print("\n[3/5] Gerando gráfico de distribuição por Subnível...")
try:
    plt.figure(figsize=(14, 8))
    subnivel_counts = df['Subnível Função2'].value_counts().head(20)
    
    # Criar gráfico de barras com cores gradientes
    colors = plt.cm.viridis(range(len(subnivel_counts)))
    subnivel_counts.plot(kind='bar', color=colors)
    
    plt.title('Top 20 Subníveis de Função', fontsize=16, fontweight='bold')
    plt.xlabel('Subnível de Função', fontsize=12)
    plt.ylabel('Quantidade', fontsize=12)
    plt.xticks(rotation=45, ha='right')
    plt.tight_layout()
    output_file = output_dir / "03_distribuicao_subnivel.png"
    plt.savefig(output_file, dpi=300, bbox_inches='tight')
    print(f"✓ Salvo: {output_file}")
    plt.close()
except Exception as e:
    print(f"✗ Erro ao gerar gráfico 3: {e}")

# Gráfico 4: Gráfico de Pizza - Agrupamento Decreto
print("\n[4/5] Gerando gráfico de pizza para Agrupamento Decreto...")
try:
    plt.figure(figsize=(10, 10))
    agrupamento_decreto = df['Agrupamento_Decreto'].value_counts()
    
    # Criar gráfico de pizza
    colors_pizza = plt.cm.Pastel1(range(len(agrupamento_decreto)))
    plt.pie(agrupamento_decreto.values, 
            labels=agrupamento_decreto.index,
            autopct='%1.1f%%',
            colors=colors_pizza,
            startangle=90)
    plt.title('Distribuição por Agrupamento Decreto', fontsize=16, fontweight='bold')
    plt.axis('equal')
    plt.tight_layout()
    output_file = output_dir / "04_pizza_agrupamento_decreto.png"
    plt.savefig(output_file, dpi=300, bbox_inches='tight')
    print(f"✓ Salvo: {output_file}")
    plt.close()
except Exception as e:
    print(f"✗ Erro ao gerar gráfico 4: {e}")

# Gráfico 5: Resumo geral - múltiplos subplots
print("\n[5/5] Gerando gráfico resumo com múltiplas visualizações...")
try:
    fig, axes = plt.subplots(2, 2, figsize=(16, 12))
    fig.suptitle('Resumo Geral - Funções e Cargos', fontsize=18, fontweight='bold')
    
    # Subplot 1: Agrupamento Geral
    agrupamento_counts = df['Agrupamento Geral'].value_counts()
    axes[0, 0].bar(range(len(agrupamento_counts)), agrupamento_counts.values, color='steelblue')
    axes[0, 0].set_title('Agrupamento Geral', fontsize=14)
    axes[0, 0].set_xticks(range(len(agrupamento_counts)))
    axes[0, 0].set_xticklabels(agrupamento_counts.index, rotation=45, ha='right')
    axes[0, 0].set_ylabel('Quantidade')
    
    # Subplot 2: Top 10 Tipos de Função
    tipo_funcao_top = df['Tipo Função Detalhada2'].value_counts().head(10)
    axes[0, 1].barh(range(len(tipo_funcao_top)), tipo_funcao_top.values, color='coral')
    axes[0, 1].set_title('Top 10 Tipos de Função', fontsize=14)
    axes[0, 1].set_yticks(range(len(tipo_funcao_top)))
    axes[0, 1].set_yticklabels(tipo_funcao_top.index, fontsize=9)
    axes[0, 1].set_xlabel('Quantidade')
    axes[0, 1].invert_yaxis()
    
    # Subplot 3: Top 10 Subníveis
    subnivel_top = df['Subnível Função2'].value_counts().head(10)
    axes[1, 0].bar(range(len(subnivel_top)), subnivel_top.values, color='mediumseagreen')
    axes[1, 0].set_title('Top 10 Subníveis de Função', fontsize=14)
    axes[1, 0].set_xticks(range(len(subnivel_top)))
    axes[1, 0].set_xticklabels(subnivel_top.index, rotation=45, ha='right')
    axes[1, 0].set_ylabel('Quantidade')
    
    # Subplot 4: Estatísticas gerais (texto)
    axes[1, 1].axis('off')
    stats_text = f"""
    ESTATÍSTICAS GERAIS
    
    Total de Registros: {len(df):,}
    
    Agrupamentos Únicos: {df['Agrupamento Geral'].nunique()}
    Tipos de Função Únicos: {df['Tipo Função Detalhada2'].nunique()}
    Subníveis Únicos: {df['Subnível Função2'].nunique()}
    
    Agrupamento mais comum:
    {agrupamento_counts.index[0]}
    ({agrupamento_counts.values[0]:,} registros)
    
    Tipo de Função mais comum:
    {tipo_funcao_top.index[0][:30]}...
    ({tipo_funcao_top.values[0]:,} registros)
    """
    axes[1, 1].text(0.1, 0.9, stats_text, fontsize=11, verticalalignment='top',
                    family='monospace', bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))
    
    plt.tight_layout()
    output_file = output_dir / "05_resumo_geral.png"
    plt.savefig(output_file, dpi=300, bbox_inches='tight')
    print(f"✓ Salvo: {output_file}")
    plt.close()
except Exception as e:
    print(f"✗ Erro ao gerar gráfico 5: {e}")

# Gerar relatório em texto
print("\n" + "=" * 60)
print("Gerando relatório em texto...")
print("=" * 60)

report_file = output_dir / "relatorio.txt"
with open(report_file, 'w', encoding='utf-8') as f:
    f.write("=" * 60 + "\n")
    f.write("RELATÓRIO DE ANÁLISE - FUNÇÕES E CARGOS\n")
    f.write("=" * 60 + "\n\n")
    
    f.write(f"Total de Registros: {len(df):,}\n\n")
    
    f.write("DISTRIBUIÇÃO POR AGRUPAMENTO GERAL:\n")
    f.write("-" * 60 + "\n")
    for idx, (key, value) in enumerate(df['Agrupamento Geral'].value_counts().items(), 1):
        f.write(f"{idx}. {key}: {value:,} ({value/len(df)*100:.2f}%)\n")
    
    f.write("\n" + "=" * 60 + "\n")
    f.write("TOP 15 TIPOS DE FUNÇÃO DETALHADA:\n")
    f.write("-" * 60 + "\n")
    for idx, (key, value) in enumerate(df['Tipo Função Detalhada2'].value_counts().head(15).items(), 1):
        f.write(f"{idx}. {key}: {value:,} ({value/len(df)*100:.2f}%)\n")
    
    f.write("\n" + "=" * 60 + "\n")
    f.write("TOP 20 SUBNÍVEIS DE FUNÇÃO:\n")
    f.write("-" * 60 + "\n")
    for idx, (key, value) in enumerate(df['Subnível Função2'].value_counts().head(20).items(), 1):
        f.write(f"{idx}. {key}: {value:,} ({value/len(df)*100:.2f}%)\n")
    
    f.write("\n" + "=" * 60 + "\n")
    f.write("DISTRIBUIÇÃO POR AGRUPAMENTO DECRETO:\n")
    f.write("-" * 60 + "\n")
    for idx, (key, value) in enumerate(df['Agrupamento_Decreto'].value_counts().items(), 1):
        f.write(f"{idx}. {key}: {value:,} ({value/len(df)*100:.2f}%)\n")

print(f"✓ Relatório salvo em: {report_file}")

# Resumo final
print("\n" + "=" * 60)
print("GRÁFICOS GERADOS COM SUCESSO!")
print("=" * 60)
print(f"\nTodos os gráficos foram salvos na pasta: {output_dir}/")
print("\nArquivos gerados:")
print("  1. 01_distribuicao_agrupamento_geral.png")
print("  2. 02_distribuicao_tipo_funcao.png")
print("  3. 03_distribuicao_subnivel.png")
print("  4. 04_pizza_agrupamento_decreto.png")
print("  5. 05_resumo_geral.png")
print("  6. relatorio.txt")
print("\n" + "=" * 60)
