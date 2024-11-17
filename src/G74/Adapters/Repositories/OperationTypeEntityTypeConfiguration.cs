using G74.DTO;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace G74.Adapters.Repositories;

public class OperationTypeEntityTypeConfiguration : IEntityTypeConfiguration<OperationTypeDataModel>
{
    public void Configure(EntityTypeBuilder<OperationTypeDataModel> builder)
    {
        builder.HasKey(u => u.Id);
        
        builder.Property(u => u.OperationTypeID)
            .HasMaxLength(50)
            .IsRequired()
            .HasColumnName("operation_type_id");

        builder.HasIndex(u => u.OperationTypeID)
            .IsUnique();
        
        builder.Property(u => u.Name)
            .IsRequired()
            .HasMaxLength(100)
            .HasColumnName("name");
        
        builder.Property(u => u.RequiredStaffBySpecialization)
            .IsRequired()
            .HasColumnName("required_staff");

        builder.Property(u => u.EstimatedDuration)
            .IsRequired()
            .HasColumnName("duration");
        
    }
}