import { Repo } from "../../core/infra/Repo";
import { MedicalCondition } from "../../domain/medicalCondition";
import { MedicalConditionId } from "../../domain/medicalConditionId";


export default interface IMedicalConditionRepo extends Repo<MedicalCondition> {

    save(medicalCondition: MedicalCondition): Promise<MedicalCondition>;
    update(medicalCondition: MedicalCondition): Promise<MedicalCondition>;
    findByDescription(description: string): Promise<MedicalCondition>;
    findAll(): Promise<MedicalCondition[]>;
    findById(id: MedicalConditionId | string): Promise<MedicalCondition>;

}