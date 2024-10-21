using G74.Domain;
using G74.Domain.Aggregates.User;
using G74.DTO;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace G74.Adapters.Repositories;

public class UserEntityTypeConfiguration : IEntityTypeConfiguration<DataUser>
{
    public void Configure(EntityTypeBuilder<DataUser> builder)
    {
        builder.HasKey(u => u.Id);
        
        builder.Property(u => u.Username)
            .HasMaxLength(50);
        
        builder.Property(u => u.Email)
            .IsRequired()
            .HasMaxLength(100);
        
        builder.Property(u => u.Role)
            .IsRequired()
            .HasMaxLength(20);
    }
}