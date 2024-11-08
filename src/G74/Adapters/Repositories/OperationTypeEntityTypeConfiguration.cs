using G74.DTO;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace G74.Adapters.Repositories;

public class OperationTypeEntityTypeConfiguration : IEntityTypeConfiguration<OperationTypeDataModel>
{
    public void Configure(EntityTypeBuilder<OperationTypeDataModel> builder)
    {
        builder.HasKey(u => u.Id);
        
        builder.Property(u => u.operationTypeID)
            .HasMaxLength(50)
            .IsRequired()
            .HasColumnName("operation_type_id");

        builder.HasIndex(u => u.operationTypeID)
            .IsUnique();
        
        builder.Property(u => u.name)
            .IsRequired()
            .HasMaxLength(100)
            .HasColumnName("name");
        
        builder.Property(u => u.requiredStaffBySpecialization)
            .IsRequired()
            .HasColumnName("required_staff");

        builder.Property(u => u.estimatedDuration)
            .IsRequired()
            .HasColumnName("duration");
        
    }
}