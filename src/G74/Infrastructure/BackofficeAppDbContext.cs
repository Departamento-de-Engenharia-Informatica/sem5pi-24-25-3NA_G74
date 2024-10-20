using G74.Adapters.Repositories;
using G74.Domain;
using Microsoft.EntityFrameworkCore;

public class BackofficeAppDbContext : DbContext
{
    
    
    
    public DbSet<Patient> Patients { get; set; }
    
    public BackofficeAppDbContext(DbContextOptions options) : base(options)
    {
    }

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        modelBuilder.ApplyConfiguration(new PatientEntityTypeConfiguration());


    }
}