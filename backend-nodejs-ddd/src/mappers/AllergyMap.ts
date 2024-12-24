import {Mapper} from "../core/infra/Mapper";
import {Allergy} from "../domain/Allergy";
import {IAllergyDTO} from "../dto/IAllergyDTO";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {Document, Model} from "mongoose";
import {IAllergyPersistence} from "../dataschema/IAllergyPersistence";

export class AllergyMap extends Mapper<Allergy> {


    public static toDTO(allergy: Allergy): IAllergyDTO {
        return {
            id: allergy.id.toString(),
            code: allergy.code,
            designation: allergy.designation,
            description: allergy.description,
        } as IAllergyDTO;
    }

    public static toDomain (raw: any | Model<IAllergyPersistence & Document>): Allergy {

        const allergyDTO: IAllergyDTO = {
            id: raw._id,
            code: raw.code,
            designation: raw.designation,
            description: raw.description
        }

        const allergyOrError = Allergy.create(
            allergyDTO,
            new UniqueEntityID(raw.domainId));

        allergyOrError.isFailure ? console.log(allergyOrError.error) : '';

        return allergyOrError.isSuccess ? allergyOrError.getValue() : null;

    }

    public static toPersistence (allergy: Allergy): any {
        return {
            domainId: allergy.id.toString(),
            code: allergy.code,
            designation: allergy.designation,
            description: allergy.description
        }
    }
}