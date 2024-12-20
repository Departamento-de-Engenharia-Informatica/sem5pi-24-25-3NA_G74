import { Result } from '../../core/logic/Result';
import IMedicalRecordDTO from '../../dto/IMedicalRecordDTO';

export default interface IMedicalRecordService {
  getAll(): Promise<Result</*x*/>>;
}
