using G74.DataModel;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace G74.Adapters.Repositories;

public class PatientEntityTypeConfiguration : IEntityTypeConfiguration<PatientDataModel>
{
    public void Configure(EntityTypeBuilder<PatientDataModel> builder)
    {
        builder.HasKey(u => u.Id);
        
        builder.HasIndex(u => u.MedicalRecordNumber)
            .IsUnique();
        
        
    }
}