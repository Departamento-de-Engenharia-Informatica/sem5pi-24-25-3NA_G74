import {Repo} from "../../core/infra/Repo";
import {Allergy} from "../../domain/Allergy";
import {AllergyId} from "../../domain/AllergyId";
import {MedicalCondition} from "../../domain/medicalCondition";

export default interface IAllergyRepo extends Repo<Allergy> {

    save(allergy: Allergy): Promise<Allergy>;
    update(allergy: Allergy): Promise<Allergy>;
    findByCode(code: string): Promise<Allergy>;
    findByDesignation(designation: string): Promise<Allergy>;
    findAll(): Promise<Allergy[]>;
    findById(id: AllergyId | string): Promise<Allergy>;
    findByDescription(description: string): Promise<Allergy>;

}