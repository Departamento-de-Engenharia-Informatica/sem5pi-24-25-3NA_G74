using G74.Domain.Aggregates.Staff;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.Staff;
using G74.Domain.Value_Objects.User;
using G74.DTO;

using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace G74.Adapters.Repositories;

public class StaffEntityTypeConfiguration : IEntityTypeConfiguration<Staff>
{
    public void Configure(EntityTypeBuilder<Staff> builder)
    {
        builder.HasKey(s => s.Id);

        builder.Property(s => s.LicenseNumber)
            .HasConversion(
                licenseNumber => licenseNumber.Value,
                value => new LicenseNumber(value))
            .IsRequired()
            .HasMaxLength(50)
            .HasColumnName("LicenseNumber");
        
        builder.Property(u => u.Name)
            .HasConversion(
                name => name.Value,
                value => new Name(value))
            .IsRequired()
            .HasMaxLength(100)
            .HasColumnName("Name");
        
        builder.Property(u => u.PhoneNumber)
            .HasConversion(
                phoneNumber => phoneNumber.Value,
                value => new PhoneNumber(value))
            .IsRequired()
            .HasMaxLength(20)
            .HasColumnName("PhoneNumber");
        
        builder.Property(u => u.ContactEmail)
            .HasConversion(
                email => email.email,
                value => new Email(value))
            .IsRequired()
            .HasMaxLength(100)
            .HasColumnName("ContactEmail");
        
        builder.Property(u => u.StaffSpecialization)
            .HasConversion(
                specialization => specialization.Value,
                value => new StaffSpecialization(value))
            .IsRequired()
            .HasMaxLength(50)
            .HasColumnName("StaffSpecialization");
            
        builder.Property(u => u.Status)
            .HasConversion(
                status => status.Value,
                value => new Status(value))
            .IsRequired()
            .HasMaxLength(50)
            .HasColumnName("Status");
        
        // // Add unique constraint on LicenseNumber
        // builder.HasIndex(s => s.LicenseNumber)
        //     .IsUnique();
    }
}