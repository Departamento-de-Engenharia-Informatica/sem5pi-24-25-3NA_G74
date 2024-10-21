using G74.Adapters.Repositories;
using G74.Domain;
using G74.Domain.Aggregates.User;
using G74.DTO;
using Microsoft.EntityFrameworkCore;

public class BackofficeAppDbContext : DbContext
{
    public BackofficeAppDbContext(DbContextOptions options) : base(options)
    {
    }

    public DbSet<Patient> Patients { get; set; }
    public DbSet<DataUser> Users { get; set; }
    
    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        modelBuilder.ApplyConfiguration(new PatientEntityTypeConfiguration());
        modelBuilder.ApplyConfiguration(new UserEntityTypeConfiguration());

    }
}