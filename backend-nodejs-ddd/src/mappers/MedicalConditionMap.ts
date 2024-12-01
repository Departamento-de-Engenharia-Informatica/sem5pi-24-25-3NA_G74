
import { Mapper } from "../core/infra/Mapper";

import { IMedicalConditionDTO } from "../dto/IMedicalConditionDTO";

import { MedicalCondition } from "../domain/medicalCondition";

import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { Document , Model } from "mongoose";
import { IMedicalConditionPersistence } from "../dataschema/IMedicalConditionPersistence";


export class MedicalConditionMap extends Mapper<MedicalCondition> {


    public static toDTO(medicalCondition: MedicalCondition): IMedicalConditionDTO {

        return {
            id: medicalCondition.id.toString(),
            description: medicalCondition.description
        } as IMedicalConditionDTO;

    }

    public static toDomain (raw: any | Model<IMedicalConditionPersistence & Document>): MedicalCondition {

        const medicalConditionOrError = MedicalCondition.create(
            raw.description,
            new UniqueEntityID(raw.domainId));

        medicalConditionOrError.isFailure ? console.log(medicalConditionOrError.error) : '';

        return medicalConditionOrError.isSuccess ? medicalConditionOrError.getValue() : null;

    }

    public static toPersistence (medicalCondition: MedicalCondition): any {
        return {
            domainId: medicalCondition.id.toString(),
            description: medicalCondition.description
        }
    }


}