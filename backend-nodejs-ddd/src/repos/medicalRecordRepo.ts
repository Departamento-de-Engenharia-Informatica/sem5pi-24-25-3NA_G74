import { Service, Inject } from 'typedi';
import MedicalRecordSchema from '../persistence/schemas/medicalRecordSchema';

@Service()
export default class MedicalRecordRepo {
  constructor(@Inject('medicalRecordSchema') private medicalRecordSchema = MedicalRecordSchema) {}

  public async findAll() {
    try {
      // Using populate to get the full allergy and condition objects instead of just IDs
      const records = await this.medicalRecordSchema.find({});
      // .populate('allergies')
      // .populate('medicalConditions');
      return records;
    } catch (err) {
      throw err;
    }
  }

  public async create(recordData) {
    try {
      // First check if a record with this patientId already exists
      const existingRecord = await this.medicalRecordSchema.findOne({ medicalRecordCode: recordData.medicalRecordCode });
      
      if (existingRecord) {
        throw new Error('Medical record already exists for this patient');
      }

      const record = new this.medicalRecordSchema(recordData);
      
      const savedRecord = await record.save();
      
      return savedRecord;
    } catch (err) {
      if (err.code === 11000) {
        
        throw new Error('Medical record already exists for this patient');
      }
      throw err;
    }
  }

  public async findById(id: string) {
    try {
      const record = await this.medicalRecordSchema.findById(id);
      return record;
    } catch (err) {
      throw err;
    }
  }

  
  public async findByPatientId(medicalRecordCode: string) {
    try {
      const record = await this.medicalRecordSchema.findOne({ medicalRecordCode });
      return record;
    } catch (err) {
      throw err;
    }
  }

  public async updateByPatientId(medicalRecordCode: string, updateData: any) {
    try {
      const updatedRecord = await this.medicalRecordSchema.findOneAndUpdate(
        { medicalRecordCode },
        { $set: updateData },
        { new: true, runValidators: true },
      );

      if (!updatedRecord) {
        throw new Error('Medical record not found for this patient');
      }

      return updatedRecord;
    } catch (err) {
      throw err;
    }
  }
}
