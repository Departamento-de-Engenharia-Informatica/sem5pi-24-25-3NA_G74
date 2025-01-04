using G74.DTO;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace G74.Adapters.Repositories;

public class SpecializationEntityTypeConfiguration : IEntityTypeConfiguration<SpecializationDataModel>
{
    public void Configure(EntityTypeBuilder<SpecializationDataModel> builder)
    {
        builder.HasKey(s => s.Id);

        builder.Property(s => s.Code)
            .IsRequired()
            .HasMaxLength(50)
            .HasColumnName("Code");

        builder.HasIndex(u => u.Code)
            .IsUnique();

        builder.Property(u => u.Designation)
            .IsRequired()
            .HasMaxLength(100)
            .HasColumnName("Designation");
    }
}