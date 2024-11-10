using G74.Adapters.Repositories;
using G74.DataModel;
using G74.Domain.Aggregates.Staff;
using G74.DTO;
using Microsoft.EntityFrameworkCore;

namespace G74.Infrastructure;

public class BackofficeAppDbContext : DbContext
{
    public BackofficeAppDbContext(DbContextOptions options) : base(options)
    {
    }

    public DbSet<PatientDataModel> Patients { get; set; }
    public DbSet<UserDataModel> Users { get; set; }
    public DbSet<Staff> Staff { get; set; }
    public DbSet<OperationRequestDataModel> OperationRequests { get; set; }
    public DbSet<OperationTypeDataModel> OperationTypeDataModel { get; set; }

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        modelBuilder.ApplyConfiguration(new UserEntityTypeConfiguration());
        modelBuilder.ApplyConfiguration(new StaffEntityTypeConfiguration());
        modelBuilder.ApplyConfiguration(new OperationRequestEntityTypeConfiguration());
        modelBuilder.ApplyConfiguration(new OperationTypeEntityTypeConfiguration());
        modelBuilder.ApplyConfiguration(new SurgeryRoomEntityTypeConfiguration());
        modelBuilder.ApplyConfiguration(new AppointmentEntityTypeConfiguration());
        modelBuilder.ApplyConfiguration(new PatientEntityTypeConfiguration());
        base.OnModelCreating(modelBuilder);
    }


}