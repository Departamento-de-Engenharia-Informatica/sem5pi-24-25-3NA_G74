import mongoose from 'mongoose';

const MedicalRecordSchema = new mongoose.Schema(
  {
    patientId: {
      type: String,
      required: true,
      unique: true,
      index: true, // Optimize queries on this field
    },
    // allergies: [
    //   {
    //     type: mongoose.Schema.Types.ObjectId,
    //     ref: 'Allergy',
    //   },
    // ],
    // medicalConditions: [
    //   {
    //     type: mongoose.Schema.Types.ObjectId,
    //     ref: 'MedicalCondition',
    //   },
    // ],
    freeText: String,
  },
  { timestamps: true },
);

export default mongoose.model('MedicalRecord', MedicalRecordSchema);
