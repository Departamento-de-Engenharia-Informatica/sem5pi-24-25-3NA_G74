using G74.Adapters.Repositories;
using G74.DataModel;
using G74.Domain;
using G74.Domain.Aggregates.User;
using G74.DTO;
using Microsoft.EntityFrameworkCore;

public class BackofficeAppDbContext : DbContext
{
    public BackofficeAppDbContext(DbContextOptions options) : base(options)
    {
    }

    public DbSet<PatientDataModel> Patients { get; set; } 
    //public virtual DbSet<DataUser> Users { get; set; } = null!;

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        modelBuilder.ApplyConfiguration(new PatientEntityTypeConfiguration());
        //modelBuilder.ApplyConfiguration(new UserEntityTypeConfiguration());
        base.OnModelCreating(modelBuilder);
    }
}