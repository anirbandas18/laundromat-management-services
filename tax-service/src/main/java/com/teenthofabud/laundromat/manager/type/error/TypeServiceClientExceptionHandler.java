package com.teenthofabud.laundromat.manager.type.error;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.teenthofabud.core.common.data.error.TOABErrorCode;
import com.teenthofabud.core.common.data.error.TOABSystemException;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.proxy.TOABBaseFeignExceptionHandler;
import feign.Response;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.io.InputStream;
import java.util.Optional;

@Component
@Slf4j
public class TypeServiceClientExceptionHandler implements TOABBaseFeignExceptionHandler<TypeException> {

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    private ObjectMapper om;

    @Override
    public Optional<TypeException> parseResponseToException(Response response) {
        Optional<TypeException> optEx = Optional.empty();
        try {
            HttpStatus httpStatusCode = HttpStatus.resolve(response.status());
            InputStream rawErrorResponseBody = response.body().asInputStream();
            ErrorVo errorDetails = om.readValue(rawErrorResponseBody, ErrorVo.class);

            if (HttpStatus.BAD_REQUEST.equals(httpStatusCode) || HttpStatus.NOT_FOUND.equals(httpStatusCode)) {
                throw new TypeException(errorDetails.getCode(), errorDetails.getMessage(), errorDetails.getDomain());
            } else {
                throw new TOABSystemException(TOABErrorCode.UNEXPECTED_CLIENT_RESPONSE_STATUS, errorDetails.getMessage());
            }

        } catch (IOException e) {
            log.error("Unable to parse response body", e);
            throw new TOABSystemException(TOABErrorCode.SYSTEM_IO_FAILURE, "Unable to parse response body", e);
        } finally {
            return optEx;
        }
    }
}
