package com.teenthofabud.laundromat.manager.type.constant;

import com.teenthofabud.core.common.data.constant.SubDomain;

public enum TypeSubDomain implements SubDomain {

    TYPE_LOV("type lov"),
    TYPE_MODEL("type model");

    private String name;

    private TypeSubDomain(String name) {
        this.name = name;
    }

    @Override
    public String getName() {
        return this.name;
    }
}
