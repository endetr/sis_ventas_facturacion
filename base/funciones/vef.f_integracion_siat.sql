CREATE OR REPLACE FUNCTION vef.f_integracion_siat (  
  p_id_venta integer,
  p_id_usuario integer
)
RETURNS varchar AS
$body$
/*
*
*  Autor:   JRR
*  DESC:    funcion que actualiza la venta con numero de factura, cuf e inserta el documento a procesarse en el sistema de SIAT
*  Fecha:   17/12/2019
*
*/

DECLARE

	v_nombre_funcion   	 text;
    v_resp    			 varchar;
    v_mensaje 			 varchar;    
    v_venta 			 record;
    v_venta_detalle 	 record;
    v_evento_significativo record;
    v_cuf                varchar;
    v_nro_factura        integer; 
    v_fecha_hora         timestamp;  
    v_tipos_masivos      varchar;
    v_tipo               varchar;
    
BEGIN

    v_nombre_funcion = 'vef.f_integracion_siat';
	  

    select p.id_periodo, v.tipo_factura as codigo_tipo_venta , v.* into v_venta
    from vef.tventa v  
    inner join param.tperiodo p on v.fecha between p.fecha_ini and p.fecha_fin      
    where id_venta = p_id_venta;

    v_tipos_masivos = pxp.f_get_variable_global_defecto('siat_tipos_masivos','');

    --obtiene fecha hora para factura
    if (now()::date = v_venta.fecha) THEN
      v_tipo = 'documento';
      v_fecha_hora = now();
    elsif (v_venta.fecha < now()::date) then
      
      --es paquete
      if (position(v_venta.codigo_tipo_venta in v_tipos_masivos) = 0 ) then
        v_tipo = 'paquete';
        if (exists (select 1 from siat.tevento_significativo where v_venta.fecha between fecha_ini::date and fecha_fin::date)) then
          select * into v_evento_significativo
          from siat.tevento_significativo 
          where v_venta.fecha between fecha_ini::date and fecha_fin::date; 
          --se pone la fecha de la factura pero la hora actual
          v_fecha_hora = to_timestamp(to_char(v_venta.fecha, 'DD/MM/YYYY') || ' ' || to_char(now(), 'HH24:MI:SS'), 'DD/MM/YYYY HH24:MI:SS');
          if (v_fecha_hora < v_evento_significativo.fecha_ini) then
            v_fecha_hora = v_evento_significativo.fecha_ini;
          elsif (v_fecha_hora > v_evento_significativo.fecha_ini) then
            v_fecha_hora = v_evento_significativo.fecha_fin;
          end if;
        else 
          raise exception 'No se puede emitir la factura a esta fecha: % debido a que no esta registrada como tipo masivo y tampoco existe un evento significativo para dicha fecha', v_venta.fecha;
        end if;
      --es masivo
      else
        v_tipo = 'masivo';
        --se pone la fecha de la factura pero la hora actual
        v_fecha_hora = to_timestamp(to_char(v_venta.fecha, 'DD/MM/YYYY') || ' ' || to_char(now(), 'HH24:MI:SS'), 'DD/MM/YYYY HH24:MI:SS');
      end if;
    end if;    
                   
    --obtiene nro de factura
      v_nro_factura = param.f_obtener_correlativo(
            'VENNF',
            v_venta.id_periodo,-- par_id periodo
            NULL, --id_uo
            NULL,    -- id_depto
            p_id_usuario,
            'VEF',
            'sin'--formato            
        )::integer;

    

    --genera cuf por ahora el cuf no se genera ya que neesitamos traer la generacion del cuf desde php
    v_cuf = 'X'; 
                 
    -- actualiza nro_factura y cuf
                  
    update vef.tventa  set 
    nro_factura =  v_nro_factura,
    cuf = v_cuf
    where id_venta  = p_id_venta;
                   
	--Validaciones****
  --validar forma de pago
  if exists (select 1
            from vef.tventa_forma_pago vfp
            inner join vef.tforma_pago fp on fp.id_forma_pago = vfp.id_forma_pago
            left join siat.tmetodo_pago mp on mp.codigo_pxp = fp.codigo 
            where vfp.id_venta = v_venta.id_venta and mp.id_metodo_pago is null) then
    raise exception 'No es posible enviar documento a sistema SIAT debido a que las formas de pago no estan correctamente mapeadas';
  end if;
 
  --validar moneda
  if not exists (select 1
            from param.tmoneda mon 
            inner join siat.ttipo_moneda m on m.codigo_pxp = mon.codigo 
            where mon.id_moneda = v_venta.id_moneda ) then
    raise exception 'No es posible enviar documento a sistema SIAT debido a que la moneda de la factura no esta correctamente mapeada';
  end if;

  --validar mapeo para el tipo de venta
  if not exists (select 1
            from vef.ttipo_venta tv
            inner join siat.tmapeo_tipo_venta m  on tv.id_tipo_venta = m.id_tipo_venta 
            where tv.codigo = v_venta.codigo_tipo_venta ) then
    raise exception 'Este tipo de venta no se encuentra mapeada en el sistema SIAT';
  end if;

  --validar url y metodo para el servicio
  if not exists (select 1
            from vef.ttipo_venta tv
            inner join siat.tmapeo_tipo_venta m  on tv.id_tipo_venta = m.id_tipo_venta 
            inner join siat.tdireccion_servicio dir on 
                dir.id_documento_sector = m.id_documento_sector and dir.id_documento_fiscal = m.id_documento_fiscal 
            where tv.codigo = v_venta.codigo_tipo_venta and dir.tipo = v_tipo) then
    raise exception 'No existe configuracion de url y metodo de servicio para este tipo de venta';
  end if;
 
  --validar detalles codigo de producto y unidad de medida	
  for v_venta_detalle in (select vd.*, pro.id_producto, um.id_unidad_medida as id_unidad_medida_siat
                          from vef.tventa_detalle vd                          
                          inner join vef.tsucursal_producto sp on sp.id_sucursal_producto = vd.id_sucursal_producto
                          inner join param.tconcepto_ingas cig on cig.id_concepto_ingas = sp.id_concepto_ingas 
                          inner join param.tunidad_medida uni on uni.id_unidad_medida = cig.id_unidad_medida
                          left join siat.tproducto pro on pro.codigo_concepto_ingas = cig.codigo
                          left join siat.tunidad_medida um on um.codigo_pxp = uni.codigo
                          where vd.id_venta = v_venta.id_venta) loop

    if (v_venta_detalle.id_producto is null) then
      raise exception 'Un producto de la venta no esta mapeado en el sistema SIAT';
    end if;

    if (v_venta_detalle.id_unidad_medida_siat is null) then
      raise exception 'Una unidad de medida no esta mapeada en el sistema SIAT';
    end if;
  end loop;
  
  --insercion en gestion documento
  INSERT INTO siat.tgestor_documento ( id_usuario_reg, tipo , id_venta , fecha_hora_factura, estado) VALUES
  (p_id_usuario,v_tipo, v_venta.id_venta, v_fecha_hora,'pendiente');


	RETURN   v_cuf;

EXCEPTION
					
	WHEN OTHERS THEN
			v_resp='';
			v_resp = pxp.f_agrega_clave(v_resp,'mensaje',SQLERRM);
			v_resp = pxp.f_agrega_clave(v_resp,'codigo_error',SQLSTATE);
			v_resp = pxp.f_agrega_clave(v_resp,'procedimientos',v_nombre_funcion);
			raise exception '%',v_resp;
END;
$body$
LANGUAGE 'plpgsql'
VOLATILE
CALLED ON NULL INPUT
SECURITY INVOKER
COST 100;