package com.teenthofabud.laundromat.manager.type.constant;

import com.teenthofabud.core.common.constant.SubDomain;

public enum TypeSubDomain implements SubDomain {

    LOV("lov"),
    MODEL("model");

    private String name;

    private TypeSubDomain(String name) {
        this.name = name;
    }

    @Override
    public String getName() {
        return this.name;
    }
}
