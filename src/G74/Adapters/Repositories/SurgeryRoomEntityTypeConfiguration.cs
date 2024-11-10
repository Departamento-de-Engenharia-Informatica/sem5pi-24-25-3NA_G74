using G74.DTO;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace G74.Adapters.Repositories;

public class SurgeryRoomEntityTypeConfiguration : IEntityTypeConfiguration<SurgeryRoomDataModel>
{
    public void Configure(EntityTypeBuilder<SurgeryRoomDataModel> builder)
    {
        builder.HasKey(u => u.Id);

        builder.Property(u => u.roomNumber)
            .IsRequired()
            .HasColumnName("room_number");
        builder.HasIndex(u => u.roomNumber)
            .IsUnique();

        builder.Property(u => u.type)
            .HasMaxLength(50)
            .IsRequired()
            .HasColumnName("type");
        
        builder.Property(u => u.capacity)
            .IsRequired()
            .HasMaxLength(100)
            .HasColumnName("capacity");
        
        builder.Property(u => u.assignedEquipment)
            .IsRequired()
            .HasColumnName("equipment");

        builder.Property(u => u.roomStatus)
            .IsRequired()
            .HasColumnName("status");

        builder.Property(u => u.maintenanceSlot)
            .IsRequired()
            .HasColumnName("maintenance");
        
    }
}