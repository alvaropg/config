/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 8; tab-width: 8 -*- */
/*
 * APP
 * Copyright (C) AUTHOR
 *
 * APP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * APP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with APP. If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef __PREFIX_OBJECT_H__
#define __PREFIX_OBJECT_H__

#include <glib-object.h>

G_BEGIN_DECLS

#define PREFIX_TYPE_OBJECT             (prefix_object_get_type())
#define PREFIX_OBJECT(obj)             (G_TYPE_CHECK_INSTANCE_CAST((obj),PREFIX_TYPE_OBJECT,PrefixObject))
#define PREFIX_OBJECT_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST((klass),PREFIX_TYPE_OBJECT,PrefixObjectClass))
#define PREFIX_IS_OBJECT(obj)          (G_TYPE_CHECK_INSTANCE_TYPE((obj),PREFIX_TYPE_OBJECT))
#define PREFIX_IS_OBJECT_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE((klass),PREFIX_TYPE_OBJECT))
#define PREFIX_OBJECT_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS((obj),PREFIX_TYPE_OBJECT,PrefixObjectClass))

typedef struct _PrefixObject        PrefixObject;
typedef struct _PrefixObjectClass   PrefixObjectClass;
typedef struct _PrefixObjectPrivate PrefixObjectPrivate;

struct _PrefixObject {
        GObject parent;

        PrefixObjectPrivate *priv;
};

struct _PrefixObjectClass {
        GObjectClass parent_class;
};

GType prefix_object_get_type (void) G_GNUC_CONST;

PrefixObject* prefix_object_new (gchar *text);
void          prefix_object_set_text (PrefixObject *self, gchar *text);
const gchar*  prefix_object_get_text (PrefixObject *self);

G_END_DECLS

#endif /* __PREFIX_OBJECT_H__ */
