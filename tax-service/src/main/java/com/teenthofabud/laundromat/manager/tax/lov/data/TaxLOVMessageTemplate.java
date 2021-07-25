package com.teenthofabud.laundromat.manager.tax.lov.data;

import lombok.Getter;

@Getter
public enum TaxLOVMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_TAX_LOV_ENTITY_ID("Searching for TaxLOVEntity with id: {}"),
    MSG_TEMPLATE_NO_TAX_LOV_ENTITY_ID_AVAILABLE("No TaxLOVEntity available with id: {}"),
    MSG_TEMPLATE_FOUND_TAX_LOV_ENTITY_ID("Found TaxLOVEntity with id: {}"),
    MSG_TEMPLATE_TAX_LOV_ID_VALID("tax LOV id: {} is semantically valid"),
    MSG_TEMPLATE_TAX_LOV_ID_INVALID("tax LOV id: {} is invalid"),
    MSG_TEMPLATE_TAX_LOV_ID_EMPTY("tax LOV id is empty"),
    MSG_TEMPLATE_TAX_LOV_EXISTENCE_BY_NAME ("Checking existence of TaxLOVEntity with name: {}"),
    MSG_TEMPLATE_TAX_LOV_EXISTS_BY_NAME ("TaxLOVEntity already exists with name: {}"),
    MSG_TEMPLATE_TAX_LOV_NON_EXISTENCE_BY_NAME ("No TaxLOVEntity exists with name: {}");

    private String value;

    private TaxLOVMessageTemplate(String value) {
        this.value = value;
    }

}
