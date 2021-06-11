package com.teenthofabud.laundromat.manager.tax.error;

import com.teenthofabud.core.common.error.TOABError;

public enum TaxErrorCode implements TOABError {

    TAX_ATTRIBUTE_INVALID("LMS-TAX-001", 400), // syntactic
    TAX_NOT_FOUND("LMS-TAX-002", 404),
    TAX_ATTRIBUTE_UNEXPECTED("LMS-TAX-003", 422), // semantic
    TAX_EXISTS("LMS-TAX-004", 409),
    TAX_INACTIVE("LMS-TAX-005", 400),
    TAX_ACTION_FAILURE("LMS-TAX-006", 500);

    private String errorCode;
    private int httpStatusCode;

    private TaxErrorCode(String errorCode, int httpStatusCode) {
        this.errorCode = errorCode;
        this.httpStatusCode = httpStatusCode;
    }

    @Override
    public String toString() {
        return "TaxErrorCode{" +
                this.name() + " -> " +
                "errorCode='" + errorCode + '\'' +
                ", httpStatusCode=" + httpStatusCode +
                '}';
    }

    @Override
    public String getErrorCode() {
        return this.errorCode;
    }

    @Override
    public Integer getHttpStatusCode() {
        return this.httpStatusCode;
    }

    @Override
    public String getDomain() {
        return "Tax";
    }
}
