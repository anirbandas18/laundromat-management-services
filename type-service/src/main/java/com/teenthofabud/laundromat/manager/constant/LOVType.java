package com.teenthofabud.laundromat.manager.constant;

import lombok.Getter;

@Getter
public enum LOVType {

    CURRENCY_TYPE("Currency Type"),
    DISTANCE_TYPE("Distance Type"),
    DURATION_TYPE("Duration Type"),
    ENTERPRISE_LEVEL("Enterprise Level"),
    GENDER_TYPE("Gender Type"),
    LAUNDRY_SERVICE("Laundry Service"),
    LOGISTIC_SERVICE("Logistic Service"),
    WEIGHT_TYPE("Weight Type");

    private String name;

    private LOVType(String name) {
        this.name = name;
    }

}
