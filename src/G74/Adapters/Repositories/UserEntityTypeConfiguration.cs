using G74.DTO;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace G74.Adapters.Repositories;

public class UserEntityTypeConfiguration : IEntityTypeConfiguration<UserDataModel>
{
    public void Configure(EntityTypeBuilder<UserDataModel> builder)
    {
        builder.HasKey(u => u.Id);

        builder.Property(u => u.Username)
            .HasMaxLength(50)
            .IsRequired()
            .HasColumnName("Username");
        
        builder.Property(u => u.Email)
            .IsRequired()
            .HasMaxLength(100)
            .HasColumnName("Email");
        
        builder.Property(u => u.Role)
            .IsRequired()
            .IsRequired()
            .HasMaxLength(20)
            .HasColumnName("Role");
    }
}