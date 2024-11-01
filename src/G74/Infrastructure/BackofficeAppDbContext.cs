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
    public DbSet<UserDataModel> Users { get; set; }
    public DbSet<StaffDataModel> Staff { get; set; }
    public DbSet<DataOperationRequest> OperationRequests { get; set; }

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        modelBuilder.ApplyConfiguration(new UserEntityTypeConfiguration());
        modelBuilder.ApplyConfiguration(new StaffEntityTypeConfiguration());
        modelBuilder.ApplyConfiguration(new OperationRequestEntityTypeConfiguration());
        base.OnModelCreating(modelBuilder);
    }
}