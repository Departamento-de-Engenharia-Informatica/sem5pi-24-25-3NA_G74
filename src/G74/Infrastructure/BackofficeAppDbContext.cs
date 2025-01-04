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
    public DbSet<StaffDataModel> Staff { get; set; }
    public DbSet<SpecializationDataModel> Specialization { get; set; }
    public DbSet<OperationRequestDataModel> OperationRequests { get; set; }
    public DbSet<OperationTypeDataModel> OperationTypes { get; set; }
    public DbSet<SurgeryRoomDataModel> SurgeryRooms { get; set; }
    public DbSet<AppointmentDataModel> Appointments { get; set; }

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        modelBuilder.ApplyConfiguration(new UserEntityTypeConfiguration());
        modelBuilder.ApplyConfiguration(new StaffEntityTypeConfiguration());
        modelBuilder.ApplyConfiguration(new SpecializationEntityTypeConfiguration());
        modelBuilder.ApplyConfiguration(new OperationRequestEntityTypeConfiguration());
        modelBuilder.ApplyConfiguration(new OperationTypeEntityTypeConfiguration());
        modelBuilder.ApplyConfiguration(new SurgeryRoomEntityTypeConfiguration());
        modelBuilder.ApplyConfiguration(new AppointmentEntityTypeConfiguration());
        modelBuilder.ApplyConfiguration(new PatientEntityTypeConfiguration());
        base.OnModelCreating(modelBuilder);
    }
}