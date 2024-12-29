import { AggregateRoot } from '../core/domain/AggregateRoot';
import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { Result } from '../core/logic/Result';
import { MedicalRecordId } from './medicalRecordId';
import IMedicalRecordDTO from '../dto/IMedicalRecordDTO';

interface MedicalRecordProps {
  allergies: string[]; // Using string[] for ObjectIds
  medicalConditions: string[];
  freeText: string;
}

export class MedicalRecord extends AggregateRoot<MedicalRecordProps> {
  get id(): UniqueEntityID {
    return this._id;
  }

  //Isto precisa ser revisto Águia
  get medicalRecordId(): MedicalRecordId {
    return new MedicalRecordId(/*this.medicalRecordId.toValue()*/);
  }

  get allergies(): string[] {
    return this.props.allergies;
  }

  get medicalConditions(): string[] {
    return this.props.medicalConditions;
  }

  get freeText(): string {
    return this.props.freeText;
  }

  set allergies(value: string[]) {
    this.props.allergies = value;
  }

  set medicalConditions(value: string[]) {
    this.props.medicalConditions = value;
  }

  set freeText(value: string) {
    this.props.freeText = value;
  }

  private constructor(props: MedicalRecordProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(medicalRecordDTO: IMedicalRecordDTO, id?: UniqueEntityID): Result<MedicalRecord> {
    // Basic validation
    if (!medicalRecordDTO) {
      return Result.fail<MedicalRecord>('Medical Record DTO is required');
    }

    const medicalRecord = new MedicalRecord(
      {
        allergies: medicalRecordDTO.allergies || [],
        medicalConditions: medicalRecordDTO.medicalConditions || [],
        freeText: medicalRecordDTO.freeText || '',
      },
      id,
    );

    return Result.ok<MedicalRecord>(medicalRecord);
  }
}
