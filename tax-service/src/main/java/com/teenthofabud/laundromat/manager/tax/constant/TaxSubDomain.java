package com.teenthofabud.laundromat.manager.tax.constant;

import com.teenthofabud.core.common.constant.SubDomain;

public enum TaxSubDomain implements SubDomain {

    MODEL("model");

    private String name;

    private TaxSubDomain(String name) {
        this.name = name;
    }

    @Override
    public String getName() {
        return this.name;
    }
}
