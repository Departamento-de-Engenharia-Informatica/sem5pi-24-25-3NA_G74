using G74.DTO;

using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace G74.Adapters.Repositories;

public class StaffEntityTypeConfiguration : IEntityTypeConfiguration<StaffDataModel>
{
    public void Configure(EntityTypeBuilder<StaffDataModel> builder)
    {
        builder.HasKey(s => s.Id);

        builder.Property(s => s.LicenseNumber)
            .IsRequired()
            .HasMaxLength(50)
            .HasColumnName("LicenseNumber");
        
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
        
        // // Add unique constraint on LicenseNumber
        // builder.HasIndex(s => s.LicenseNumber)
        //     .IsUnique();
    }
}