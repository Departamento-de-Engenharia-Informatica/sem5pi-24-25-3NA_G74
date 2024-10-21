using G74.Adapters.Repositories;
using G74.Domain;
using G74.Domain.Aggregates.User;
using G74.DTO;
using Microsoft.EntityFrameworkCore;

public class BackofficeAppDbContext : DbContext
{
    public BackofficeAppDbContext(DbContextOptions options) : base(options)
    {
        Database.EnsureCreated();
    }

    public virtual DbSet<Patient> Patients { get; set; } = null!;
    public virtual DbSet<DataUser> Users { get; set; } = null!;
    
    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        modelBuilder.ApplyConfiguration(new PatientEntityTypeConfiguration());
        modelBuilder.ApplyConfiguration(new UserEntityTypeConfiguration());
        base.OnModelCreating(modelBuilder);
    }
}