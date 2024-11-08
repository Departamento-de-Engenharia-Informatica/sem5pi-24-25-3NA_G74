using G74.DTO;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace G74.Adapters.Repositories;

public class AppointmentEntityTypeConfiguration : IEntityTypeConfiguration<AppointmentDataModel>
{
    public void Configure(EntityTypeBuilder<AppointmentDataModel> builder)
    {
        builder.HasKey(u => u.Id);
        
        
    }
}