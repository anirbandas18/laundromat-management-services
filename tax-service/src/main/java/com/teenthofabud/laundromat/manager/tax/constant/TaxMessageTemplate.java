package com.teenthofabud.laundromat.manager.tax.constant;

public final class TaxMessageTemplate {

    private TaxMessageTemplate() {

    }

    public static final String MSG_TEMPLATE_SEARCHING_FOR_TAX_MODEL_ENTITY_ID = "Searching for TaxModelEntity with id: {}";
    public static final String MSG_TEMPLATE_NO_TAX_MODEL_ENTITY_ID_AVAILABLE = "No TaxModelEntity available with id: {}";
    public static final String MSG_TEMPLATE_FOUND_TAX_MODEL_ENTITY_ID = "Found TaxModelEntity with id: {}";

    public static final String MSG_TEMPLATE_TAX_MODEL_ID_VALID = "tax model id: {} is semantically valid";
    public static final String MSG_TEMPLATE_TAX_MODEL_ID_INVALID = "tax model id: {} is invalid";
    public static final String MSG_TEMPLATE_TAX_MODEL_ID_EMPTY = "tax model id is empty";

    public static final String MSG_TEMPLATE_TAX_MODEL_DTO_ID_INVALID = "TaxModelDto.currencyTypeModel.id is invalid";
}
