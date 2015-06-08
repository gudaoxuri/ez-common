package com.fasterxml.jackson.core.type;

import java.lang.reflect.Field;

public class STypeReference<T> extends TypeReference<T> {
    public STypeReference(T t) throws IllegalArgumentException, IllegalAccessException, SecurityException, NoSuchFieldException {
        Class<?> cla = TypeReference.class;
        Field field = cla.getDeclaredField("_type");
        field.setAccessible(true);
        field.set(this, t);
    }
}
