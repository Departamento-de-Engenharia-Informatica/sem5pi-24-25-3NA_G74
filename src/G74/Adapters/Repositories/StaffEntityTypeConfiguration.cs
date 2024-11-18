using G74.Domain.Aggregates.Staff;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.Staff;
using G74.Domain.Value_Objects.User;
using G74.DTO;

using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace G74.Adapters.Repositories;

public class StaffEntityTypeConfiguration : IEntityTypeConfiguration<StaffDataModel>
{
    public void Configure(EntityTypeBuilder<StaffDataModel> builder)
    {
        builder.HasKey(s => s.Id);

        builder.Property(s => s.LicenceNumber)
            .IsRequired()
            .HasMaxLength(50)
            .HasColumnName("LicenceNumber");

        builder.HasIndex(u => u.LicenceNumber)
            .IsUnique();

        builder.Property(u => u.Name)
            .IsRequired()
            .HasMaxLength(100)
            .HasColumnName("Name");

        builder.Property(u => u.PhoneNumber)
            .IsRequired()
            .HasMaxLength(20)
            .HasColumnName("PhoneNumber");

        builder.Property(u => u.ContactEmail)
            .IsRequired()
            .HasMaxLength(100)
            .HasColumnName("ContactEmail");

        builder.Property(u => u.StaffSpecialization)
            .IsRequired()
            .HasMaxLength(50)
            .HasColumnName("StaffSpecialization");

        builder.Property(u => u.Status)
            .IsRequired()
            .HasMaxLength(50)
            .HasColumnName("Status");

        builder.Property(u => u.Availability)
            .IsRequired()
            .HasMaxLength(50)
            .HasColumnName("availability");
    }
}