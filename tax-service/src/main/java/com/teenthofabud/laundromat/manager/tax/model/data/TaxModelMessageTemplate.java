package com.teenthofabud.laundromat.manager.tax.model.data;

import lombok.Getter;

@Getter
public enum TaxModelMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_TAX_MODEL_ENTITY_ID("Searching for TaxModelEntity with id: {}"),
    MSG_TEMPLATE_NO_TAX_MODEL_ENTITY_ID_AVAILABLE("No TaxModelEntity available with id: {}"),
    MSG_TEMPLATE_FOUND_TAX_MODEL_ENTITY_ID("Found TaxModelEntity with id: {}"),

    MSG_TEMPLATE_TAX_MODEL_ID_VALID("tax lov id: {} is semantically valid"),
    MSG_TEMPLATE_TAX_MODEL_ID_INVALID("tax lov id: {} is invalid"),
    MSG_TEMPLATE_TAX_MODEL_ID_EMPTY("tax lov id is empty"),

    MSG_TEMPLATE_TAX_MODEL_DTO_CURRENCY_TYPE_MODEL_ID_INVALID("TaxModelDto.currencyTypeModel.id is invalid"),
    MSG_TEMPLATE_TAX_MODEL_DTO_TAX_LOV_ID_INVALID("TaxModelDto.taxLovId is invalid"),

    MSG_TEMPLATE_TAX_MODEL_EXISTENCE_BY_NAME_AND_TAX_LOV_ID ("Checking existence of TypeModelEntity with name: {} and taxLovId: {}"),
    MSG_TEMPLATE_TAX_MODEL_EXISTS_BY_NAME_AND_TAX_LOV_ID ("TypeModelEntity already exists with name: {} and taxLovId: {}"),
    MSG_TEMPLATE_TAX_MODEL_NON_EXISTENCE_BY_NAME_AND_TAX_LOV_ID ("No TypeModelEntity exists with name: {} and taxLovId: {}"),

    MSG_TEMPLATE_TAX_MODEL_EXISTENCE_BY_NAME_AND_CURRENCY_TYPE_MODEL_ID ("Checking existence of TypeModelEntity with name: {} and currencyTypeModel.id: {}"),
    MSG_TEMPLATE_TAX_MODEL_EXISTS_BY_NAME_AND_CURRENCY_TYPE_MODEL_ID ("TypeModelEntity already exists with name: {} and currencyTypeModel.id: {}"),
    MSG_TEMPLATE_TAX_MODEL_NON_EXISTENCE_BY_NAME_AND_CURRENCY_TYPE_MODEL_ID ("No TypeModelEntity exists with name: {} and currencyTypeModel.id: {}"),

    MSG_TEMPLATE_TAX_MODEL_EXISTENCE_BY_NAME_AND_TAX_LOV_ID_AND_CURRENCY_TYPE_MODEL_ID("Checking existence of TypeModelEntity with name: {} and taxLovId: {} and currencyTypeModel.id: {}"),
    MSG_TEMPLATE_TAX_MODEL_EXISTS_BY_NAME_AND_TAX_LOV_ID_AND_CURRENCY_TYPE_MODEL_ID("TypeModelEntity already exists with name: {} and taxLovId: {} and currencyTypeModel.id: {}"),
    MSG_TEMPLATE_TAX_MODEL_NON_EXISTENCE_BY_NAME_AND_TAX_LOV_ID_AND_CURRENCY_TYPE_MODEL_ID("No TypeModelEntity exists with name: {} and taxLovId: {} and currencyTypeModel.id: {}"),
    MSG_TEMPLATE_TAX_MODEL_DTO_TAX_LOV_ID_INACTIVE("TaxModelDto.taxLovId is inactive"),
    MSG_TEMPLATE_TAX_MODEL_DTO_CURRENCY_TYPE_MODEL_ID_INACTIVE("TaxModelDto.currencyTypeModelId is inactive");


    private String value;

    private TaxModelMessageTemplate(String value) {
        this.value = value;
    }
    
}
