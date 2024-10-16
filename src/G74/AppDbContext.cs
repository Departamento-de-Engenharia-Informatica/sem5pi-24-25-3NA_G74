
using G74;
using Microsoft.EntityFrameworkCore;

public class AppDbContext : DbContext
{
    public AppDbContext(DbContextOptions<AppDbContext> options) : base(options) { }

    public DbSet<G74.Produto> Produtos { get; set; }

    protected override void OnModelCreating(ModelBuilder modelBuilder)
{
    modelBuilder.Entity<Produto>()
        .Property(p => p.Preco)
        .HasPrecision(18, 4); // or whatever precision and scale is appropriate
}
}
