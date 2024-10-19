using G74;
using Microsoft.EntityFrameworkCore;

public class BackofficeAppDbContext : DbContext
{
    public BackofficeAppDbContext(DbContextOptions options) : base(options)
    {
    }

    public DbSet<G74.Produto> Produtos { get; set; }

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        
        
        // Abaixo, esta maneira de fazer vai ter que ser mudada, usar .ApplyConfiguration e meter uma classe XEntityTypeConfiguration
        modelBuilder.Entity<Produto>()
            .Property(p => p.Preco)
            .HasPrecision(18, 4); // or whatever precision and scale is appropriate
    }
}